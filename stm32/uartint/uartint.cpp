//  UART Interface for STM32 UART port. Compatible with Arduino's SoftwareSerial.
#define SIMULATE_WISOL //  Simulate a Wisol Sigfox module connected to UART.
#include <string.h>
#include <bluepill.h>
#include <logger.h>
#include "uartint.h"

//  TODO: Implement a real UART interface with interrupts based on
//  https://github.com/libopencm3/libopencm3-examples/blob/master/examples/stm32/f1/stm32-maple/usart_irq/usart_irq.c

#ifdef SIMULATE_WISOL //  Simulate a Wisol Sigfox module connected to UART.
//  Message limits from https://github.com/lupyuen/send_altitude_cocoos/blob/master/platform.h
#define MAX_UART_SEND_MSG_SIZE 35  //  Max message length, e.g. 33 chars for AT$SF=0102030405060708090a0b0c,1\r
#define MAX_UART_RESPONSE_MSG_SIZE 36  //  Max response length, e.g. 36 chars for ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT\r

//  Command timeouts from https://github.com/lupyuen/send_altitude_cocoos/blob/master/sigfox.h
#define COMMAND_TIMEOUT ((unsigned long) 10 * 1000)  //  Wait up to 10 seconds for simple command response from Sigfox module.
#define UPLINK_TIMEOUT ((unsigned long) 20 * 1000)  //  Wait up to 20 seconds for uplink command response from Sigfox module.
#define DOWNLINK_TIMEOUT ((unsigned long) 60 * 1000)  //  Wait up to 60 seconds for downlink command response from Sigfox module.

//  Wisol AT Commands from https://github.com/lupyuen/send_altitude_cocoos/blob/master/wisol.cpp

///////////////////////////////////////////////////////////////////////////////
//  Define the Wisol AT Commands based on WISOLUserManual_EVBSFM10RxAT_Rev.9_180115.pdf

#define END_OF_RESPONSE '\r'  //  Character '\r' marks the end of response.
#define CMD_END "\r"

#define CMD_NONE "AT"  //  Empty placeholder command.
#define CMD_OUTPUT_POWER_MAX "ATS302=15"  //  For RCZ1: Set output power to maximum power level.
#define CMD_GET_CHANNEL "AT$GI?"  //  For RCZ2, 4: Get current and next TX macro channel usage.  Returns X,Y.
#define CMD_RESET_CHANNEL "AT$RC"  //  For RCZ2, 4: Reset default channel. Send this command if CMD_GET_CHANNEL returns X=0 or Y<3.
#define CMD_SEND_MESSAGE "AT$SF="  //  Prefix to send a message to SIGFOX cloud.
#define CMD_SEND_MESSAGE_RESPONSE ",1"  //  Append to payload if downlink response from Sigfox is needed.
#define CMD_GET_ID "AT$I=10"  //  Get Sigfox device ID.
#define CMD_GET_PAC "AT$I=11"  //  Get Sigfox device PAC, used for registering the device.
#define CMD_EMULATOR_DISABLE "ATS410=0"  //  Device will only talk to Sigfox network.
#define CMD_EMULATOR_ENABLE "ATS410=1"  //  Device will only talk to SNEK emulator.

#ifdef NOTUSED  //  For future use.
#define CMD_GET_TEMPERATURE "AT$T?"  //  Get the module temperature.
#define CMD_GET_VOLTAGE "AT$V?"  //  Get the module voltage.

#define CMD_RESET "AT$P=0"  //  Software reset.
#define CMD_SLEEP "AT$P=1"  //  Switch to sleep mode : consumption is < 1.5uA
#define CMD_WAKEUP "AT$P=0"  //  Switch back to normal mode : consumption is 0.5 mA

#define CMD_RCZ1 "AT$IF=868130000"  //  Set EU / RCZ1 Frequency.
#define CMD_RCZ2 "AT$IF=902200000"  //  Set US / RCZ2 Frequency.
#define CMD_RCZ3 "AT$IF=902080000"  //  Set JP / RCZ3 Frequency.
#define CMD_RCZ4 "AT$IF=920800000"  //  Set RCZ4 Frequency.
#define CMD_MODULATION_ON "AT$CB=-1,1"  //  Modulation wave on.
#define CMD_MODULATION_OFF "AT$CB=-1,0"  //  Modulation wave off.
#endif

static char command[MAX_UART_SEND_MSG_SIZE + 1];   //  AT command being received from the caller.
static char data[MAX_UART_RESPONSE_MSG_SIZE + 1];  //  Simulated data to be returned for the response.
static uint8_t dataLength = 0;  //  Number of simulated bytes in data to be returned.
static uint8_t dataIndex = 0;   //  Current index to the simulated data to be returned.
static uint32_t dataTimestamp = 0;  //  Response will only be delivered at this timestamp, to simulate delay.

void UARTInterface::begin(uint16_t bps) {
    //  Erase the command buffer.
    command[0] = 0;
    dataIndex = 0;
    dataLength = 0;
    dataTimestamp = 0;
}

int UARTInterface::available() { 
    //  Return the number of simulated bytes to be read from the UART port.
    if (dataIndex >= dataLength) { return 0; }  //  No data.
    if (millis() < dataTimestamp) { return 0; }  //  Not ready, wait for delay.
    return dataLength - dataIndex;
}

int UARTInterface::read() { 
    //  Return the next simulated byte to be read from the UART port. Or return -1 if none.
    //  debug_println("uart_read");
    if (available() == 0) { return -1; }  //  No data or not ready.
    return data[dataIndex++];
}

static void simulateCommand(const char *cmd) {
    data[0] = 0;
    dataIndex = 0;
    dataLength = 0;
    dataTimestamp = millis() + 2000;  //  Delay 2 seconds by default.
    const char *response = "OK";  //  Default response.
    if (strcmp(cmd, CMD_NONE) == 0) {  //  "AT": Empty placeholder command.
        //  Default to "OK".
    } else if (strcmp(cmd, CMD_OUTPUT_POWER_MAX) == 0) {  //  "ATS302=15": For RCZ1: Set output power to maximum power level.
        //  Default to "OK".
    } else if (strcmp(cmd, CMD_GET_CHANNEL) == 0) {  //  "AT$GI?": For RCZ2, 4: Get current and next TX macro channel usage.  Returns X,Y.
        response = "0,3";
    } else if (strcmp(cmd, CMD_RESET_CHANNEL) == 0) {  //  "AT$RC": For RCZ2, 4: Reset default channel. Send this command if CMD_GET_CHANNEL returns X=0 or Y<3.
        //  Default to "OK".
    } else if (strcmp(cmd, CMD_GET_ID) == 0) {  //  "AT$I=10": Get Sigfox device ID.
        response = "002C2EA1";  //  Must be 6 chars.
    } else if (strcmp(cmd, CMD_GET_PAC) == 0) {  //  "AT$I=11": Get Sigfox device PAC, used for registering the device.
        response = "5BEB8CF64E869BD1";  //  Must be 16 chars.
    } else if (strcmp(cmd, CMD_EMULATOR_DISABLE) == 0) {  //  "ATS410=0": Device will only talk to Sigfox network.
        //  Default to "OK".
    } else if (strcmp(cmd, CMD_EMULATOR_ENABLE) == 0) {  //  "ATS410=1": Device will only talk to SNEK emulator.
        debug_print("***** Error: Emulation mode not supported");
    } else if (strncmp(cmd, CMD_SEND_MESSAGE, strlen(CMD_SEND_MESSAGE)) == 0) {  
        //  "AT$SF=": Prefix to send a message to Sigfox.
        //  If it ends with ",1", request for downlink.
        int pos = strlen(cmd) - strlen(CMD_SEND_MESSAGE_RESPONSE);
        if (strcmp(cmd + pos, CMD_SEND_MESSAGE_RESPONSE) == 0) {
            //  Downlink.  Return response 1 min later.
            response = "OK\r\nRX=01 23 45 67 89 AB CD EF";
            dataTimestamp = millis() + DOWNLINK_TIMEOUT;
        } else {
            //  No downlink.  Default to "OK" after send delay.
            dataTimestamp = millis() + UPLINK_TIMEOUT;
        }
    } else {
        debug_print("***** Error: Unknown Wisol cmd ");
        debug_println(cmd);
    }    
    int responseLength = strlen(response);
    if (responseLength >= MAX_UART_RESPONSE_MSG_SIZE - 1) {
        debug_println("***** Error: Wisol response overflow");
        return;        
    }
    strncpy(data, response, MAX_UART_RESPONSE_MSG_SIZE);
    data[MAX_UART_RESPONSE_MSG_SIZE] = 0;  //  Terminate the response in case of overflow.
    data[responseLength++] = END_OF_RESPONSE;  //  End the '\r' char.
    data[responseLength] = 0;
    dataLength = strlen(data);
}

void UARTInterface::write(uint8_t ch) {
    //  Simulate the handling of a command char sent to the Wisol module via the UART port.
    //  debug_println("uart_write");
    if (ch != CMD_END[0]) {
        //  Not end of command. Append the character.
        int commandLength = strlen(command);
        if (commandLength >= MAX_UART_SEND_MSG_SIZE) {
            debug_println("****** Error: Wisol cmd overflow");
            return;
        }
        command[commandLength++] = ch;
        command[commandLength] = 0;  //  Terminate the string.
        return;
    }
    //  End of command found. Process it and respond after a delay.
    simulateCommand(command);
    command[0] = 0;  //  Erase the command.
}

#endif  // SIMULATE_WISOL

UARTInterface::UARTInterface(unsigned rx, unsigned tx) {
    //  TODO: Init the UART port connected to the receive/transmit pins.
}

void UARTInterface::listen() {
    //  TODO: Listen to incoming data from the UART port.
}

void UARTInterface::end() {
    //  TODO: Close the UART port.
}
