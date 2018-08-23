//  Defines functions specific to the STM32 platform.
#include <Wire.h>

I2CInterface Wire;  //  Used by BME280 library.

//  Functions specific to the platform, e.g. STM32.  Called by main.cpp.
extern "C" void platform_setup(void);  //  Initialise the STM32 platform.
extern "C" void platform_start_timer(void);  //  Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.
extern "C" int test_main(void);  //  WARNING: test_main() never returns.

void platform_setup(void) {
    //  TODO: Initialise the STM32 platform.

    //  TODO: Do some STM32 testing for now. Will be removed.
    test_main();  //  WARNING: test_main() never returns.
}

void platform_start_timer(void) {
    //  TODO: Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.
}

//////////////////////////////////////////////////////////////////////////
//  STM32 Blue Pill Testing. 

#ifdef NOTUSED
//  ARM Semihosting code from http://www.keil.com/support/man/docs/ARMCC/armcc_chr1359125001592.htm
//  https://wiki.dlang.org/Minimal_semihosted_ARM_Cortex-M_%22Hello_World%22

int __semihost(int cmd, const void *msg) {
    asm {
      "mov r0, %[cmd]; 
       mov r1, %[msg]; 
       bkpt #0xAB"
	:                              
	: [cmd] "r" command, [msg] "r" message
	: "r0", "r1", "memory";
    }
}

//  ARM Semihosting code from https://github.com/ARMmbed/mbed-os/blob/master/platform/mbed_semihost_api.c

//  ARM Semihosting Commands
#define SYS_OPEN   (0x1)
#define SYS_CLOSE  (0x2)
#define SYS_WRITE  (0x5)
#define SYS_READ   (0x6)
#define SYS_ISTTY  (0x9)
#define SYS_SEEK   (0xa)
#define SYS_ENSURE (0xb)
#define SYS_FLEN   (0xc)
#define SYS_REMOVE (0xe)
#define SYS_RENAME (0xf)
#define SYS_EXIT   (0x18)

int semihost_write(uint32_t fh, const unsigned char *buffer, unsigned int length, int mode)
{
    if (length == 0) { return 0; }
    uint32_t args[3];
    args[0] = (uint32_t)fh;
    args[1] = (uint32_t)buffer;
    args[2] = (uint32_t)length;
    return __semihost(SYS_WRITE, args);
}
#endif

//  Blink code from https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC

#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>

static void gpio_setup(void) {

	/* Enable GPIOC clock. */
	rcc_periph_clock_enable(RCC_GPIOC);

	/* Set GPIO8 (in GPIO port C) to 'output push-pull'. */
	gpio_set_mode(GPIOC,GPIO_MODE_OUTPUT_2_MHZ,
		      GPIO_CNF_OUTPUT_PUSHPULL,GPIO13);
}

int test_main(void) {
	//  We blink the Blue Pill onboard LED in a special pattern to distinguish ourselves
	//  from other blink clones - 2 x on, then 1 x off.
	int i;

	gpio_setup();

	for (;;) {
		gpio_clear(GPIOC,GPIO13);	/* LED on */
		for (i = 0; i < 1500000; i++)	/* Wait a bit. */
			__asm__("nop");

		gpio_clear(GPIOC,GPIO13);	/* LED on */
		for (i = 0; i < 1500000; i++)	/* Wait a bit. */
			__asm__("nop");

		gpio_set(GPIOC,GPIO13);		/* LED off */
		for (i = 0; i < 500000; i++)	/* Wait a bit. */
			__asm__("nop");
	}

	return 0;
}
