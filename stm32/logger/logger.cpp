//  Log messages to the debug console.  We use ARM Semihosting to display messages.
#include "logger.h"
#include <string.h>

#define DEBUG_BUFFER_SIZE 80
static char debugBuffer[DEBUG_BUFFER_SIZE + 1];  //  Buffer to hold output before flushing.
static bool logEnabled = false;  //  Logging is off by default.  Developer must switch it on with enable_debug().

void enable_log(void) { logEnabled = true; debugBuffer[0] = 0; }
void disable_log(void) { logEnabled = false; debugBuffer[0] = 0; }

//  ARM Semihosting code from 
//  http://www.keil.com/support/man/docs/ARMCC/armcc_pge1358787046598.htm
//  http://www.keil.com/support/man/docs/ARMCC/armcc_pge1358787048379.htm
//  http://www.keil.com/support/man/docs/ARMCC/armcc_chr1359125001592.htm
//  https://wiki.dlang.org/Minimal_semihosted_ARM_Cortex-M_%22Hello_World%22

static int __semihost(int command, void* message) {
	//  Send an ARM Semihosting command to the debugger, e.g. to print a message.
	//  To see the message you need to run opencd and gdb concurrently:
	//    openocd -f interface/stlink-v2.cfg -f target/stm32f1x.cfg
	//    arm-none-eabi-gdb -x loader.gdb

	//  Warning: This code will trigger a breakpoint and hang unless a debugger is connected.
	//  That's how ARM Semihosting sends a command to the debugger to print a message.
	//  This code MUST be disabled on production devices.
    if (!logEnabled) return -1;
    __asm( 
      "mov r0, %[cmd] \n"
      "mov r1, %[msg] \n" 
      "bkpt #0xAB \n"
	:  //  Output operand list: (nothing)
	:  //  Input operand list:
		[cmd] "r" (command), 
		[msg] "r" (message)
	:  //  Clobbered register list:
		"r0", "r1", "memory"
	);
	return 0;  //  TODO
}

//  ARM Semihosting code from https://github.com/ARMmbed/mbed-os/blob/master/platform/mbed_semihost_api.c

// ARM Semihosting Commands
// #define SYS_OPEN   (0x1)
// #define SYS_CLOSE  (0x2)
#define SYS_WRITE  (0x5)
// #define SYS_READ   (0x6)
// #define SYS_ISTTY  (0x9)
// #define SYS_SEEK   (0xa)
// #define SYS_ENSURE (0xb)
// #define SYS_FLEN   (0xc)
// #define SYS_REMOVE (0xe)
// #define SYS_RENAME (0xf)
// #define SYS_EXIT   (0x18)

// We normally set the file handle to 2 to write to the debugger's stderr output.
#define SEMIHOST_HANDLE 2

static int semihost_write(uint32_t fh, const unsigned char *buffer, unsigned int length) {
    //  Write "length" number of bytes from "buffer" to the debugger's file handle fh.
    //  We normally set fh=2 to write to the debugger's stderr output.
    if (length == 0) { return 0; }
    uint32_t args[3];
    args[0] = (uint32_t)fh;
    args[1] = (uint32_t)buffer;
    args[2] = (uint32_t)length;
    return __semihost(SYS_WRITE, args);
}

static void debug_append(const char *buffer, unsigned int length) {
    //  Append "length" number of bytes from "buffer" to the debug buffer.
    const int debugBufferLength = strlen(debugBuffer);
    //  If can't fit into buffer, just send to the debugger log now.
    if (debugBufferLength + length >= DEBUG_BUFFER_SIZE) {
        debug_flush();
        semihost_write(SEMIHOST_HANDLE, (const unsigned char *) buffer, length);
        return;
    }
    //  Else append to the buffer.
    strncat(debugBuffer, buffer, length);
    debugBuffer[debugBufferLength + length] = 0;  //  Terminate the string.
}

void debug_flush(void) {
    //  Flush the debug buffer to the debugger log.  This will be slow.
    if (debugBuffer[0] == 0) { return; }  //  Debug buffer is empty, nothing to write.
	semihost_write(SEMIHOST_HANDLE, (const unsigned char *) debugBuffer, strlen(debugBuffer));
    debugBuffer[0] = 0;
}

void debug_print(size_t l) {
    //  We only print up to 10 digits, since 32 bits will give max 4,294,967,296.    
    //  char buf[32]; sprintf(buf, "%u", l); debug_append(buf, strlen(buf)); return; ////  TODO
    
    #define MAX_INT_LENGTH 10
    char buffer[MAX_INT_LENGTH + 1];
    int size = MAX_INT_LENGTH + 1;
    bool prefixByZero = false;
    int length = 0;
    for(size_t divisor = 1000000000ul; divisor >= 1; divisor = divisor / 10) {
        char digit = '0' + (char)(l / divisor);
        if (digit > '9') {
            debug_print("(Overflow)");
            return;
        }
        if (digit > '0' || length > 0 || prefixByZero) {
            if (length < size) {
                buffer[length++] = digit;
            }
        }
        l = l % divisor;
    }
    if (length == 0) { buffer[length++] = '0'; };
    if (length < size) buffer[length] = 0;
    buffer[size - 1] = 0;  //  Terminate in case of overflow.

    debug_append(buffer, strlen(buffer));
}

void debug_print(int i) {
    if (i == 0) { debug_append("0", 1); } 
    else if (i >= 0) { debug_print((size_t) i); }
    else {  // i < 0.
        debug_append("-", 1);
        debug_print((size_t) -i);
    }
}

void debug_print(float f) {
    //  Assume max 10 digits to the left of decimal point and 2 digits to the right.
    if (f == 0) {
        debug_append("0.00", 4);
        return;
    } else if (f < 0) {
        debug_append("-", 1);
        f = -f;
    }
    //  Print the digits left of the decimal point.
    debug_print((size_t) f);
    debug_append(".", 1);
    //  Print the 2 digits right of the decimal point.
    f = f * 10.0;
    debug_print(((size_t) f) % 10);
    f = f * 10.0;
    debug_print(((size_t) f) % 10);
}

void debug_begin(uint16_t bps) {
    //  TODO
}

void debug_write(uint8_t ch) {
	debug_append((const char *) &ch, 1);
}

void debug_print(const char *s) {
    if (s[0] == 0) return;
	debug_append(s, strlen(s));
}

void debug_println(const char *s) {
    if (s[0] != 0) { debug_print(s); }
    debug_append("\n", 1);
}

void debug_print(char ch) {
	debug_append(&ch, 1);
}

void debug_println(int i) {
    debug_print(i);
    debug_append("\n", 1);
}

void debug_println(size_t l) {
    debug_print(l);
    debug_append("\n", 1);
}

void debug_println(char ch) {
    debug_print(ch);
    debug_append("\n", 1);
}

void debug_println(float f) {
    debug_print(f);
    debug_append("\n", 1);
}
