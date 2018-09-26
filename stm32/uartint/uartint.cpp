//  UART Interface for STM32 Blue Pill UART port, with interrupts. Compatible with Arduino's SoftwareSerial.
//  Based on https://github.com/libopencm3/libopencm3-examples/blob/master/examples/stm32/f1/stm32-maple/usart_irq/usart_irq.c
#include <boost/lockfree/spsc_queue.hpp>  //  Somehow must appear before platform.h.
#include "../../platform.h"  //  For SIMULATE_WISOL, MAX_UART_SEND_MSG_SIZE
#include <string.h>
#include <bluepill.h>
#include <logger.h>
#include "uartint.h"

#ifndef SIMULATE_WISOL  //  If we are using a real Wisol Sigfox module connected to the UART port...
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/usart.h>
#include <libopencm3/cm3/nvic.h>
#include <libopencm3/cm3/scb.h>

//  Allocate the response queue, a fixed size lockfree circular ringbuffer for receiving data.
//  UART interrupts may happen anytime, so we need a lockfree way to access the buffer safely.
static boost::lockfree::spsc_queue<
    uint8_t,  //  Response queue stores an array of bytes.
    boost::lockfree::capacity<MAX_UART_RESPONSE_MSG_SIZE + 1>  //  Max response size.
> responseQueue;

static void clock_setup(void) {
    //  Enable the USART2 clock.

    //  Moved to platform_setup() in bluepill.cpp.
	//  rcc_clock_setup_in_hse_8mhz_out_72mhz();
	//  Enable GPIOA clock.
	rcc_periph_clock_enable(RCC_GPIOA);
	//  Enable USART2 clock.
	rcc_periph_clock_enable(RCC_USART2);
}

static void usart_setup(uint16_t bps) {
    //  Configure the USART2 port for "bps" bits per second,
    //  8 data bits, No parity, 1 stop bit.  Port is not enabled
    //  until listen() is called.

	//  Setup GPIO pin GPIO_USART2_RE_TX on GPIO port A for transmit.
	gpio_set_mode(GPIOA, GPIO_MODE_OUTPUT_50_MHZ,
		      GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, GPIO_USART2_TX);
	//  Setup GPIO pin GPIO_USART2_RE_RX on GPIO port A for receive.
	gpio_set_mode(GPIOA, GPIO_MODE_INPUT,
		      GPIO_CNF_INPUT_FLOAT, GPIO_USART2_RX);
	//  Setup UART parameters.
	usart_set_baudrate(USART2, bps);
	usart_set_databits(USART2, 8);
	usart_set_parity(USART2, USART_PARITY_NONE);
	usart_set_stopbits(USART2, USART_STOPBITS_1);
	usart_set_flow_control(USART2, USART_FLOWCONTROL_NONE);
	usart_set_mode(USART2, USART_MODE_TX_RX);
}

void usart2_isr(void) {
    //  Interrupt service routine for USART2. We enqueue each byte received.
	//  Check if we were called because of received data (RXNE). */
	if (((USART_CR1(USART2) & USART_CR1_RXNEIE) != 0) &&
	    ((USART_SR(USART2) & USART_SR_RXNE) != 0)) {
		//  Read the next received byte and add to response queue.
		uint8_t ch = usart_recv(USART2);
        responseQueue.push(ch);
	}
}

UARTInterface::UARTInterface(unsigned rx, unsigned tx) {
    //  Init the UART interface.  This constructor is called before platform_setup(),
    //  so we don't do any setup yet till later.
}

void UARTInterface::begin(uint16_t bps) {
    //  Init the buffer.
    debug_println("[[ uart begin"); debug_flush();
    responseQueue.reset();
    //  Open the UART port.
    clock_setup();
    usart_setup(bps);
}

void UARTInterface::listen() {
    //  Start receiving incoming data from the UART port.  Set up the receive interrupt.
    //  debug_println("uart_listen"); debug_flush();

	//  Enable the USART2 interrupt.
	nvic_enable_irq(NVIC_USART2_IRQ);
	//  Enable USART2 Receive (RXNE) interrupt so we are notified when a byte is received.
	USART_CR1(USART2) |= USART_CR1_RXNEIE;
	//  Finally enable the USART.
	usart_enable(USART2);
}

void UARTInterface::end() {
    //  Close the UART port.  Disable interrupts.
    //  Disable USART2 Transmit (TXE) interrupt.
	USART_CR1(USART2) &= ~USART_CR1_TXEIE;
    //  Disable USART2 Receive (RXNE) interrupt.
	USART_CR1(USART2) &= ~USART_CR1_RXNEIE;
    //  Disable the USART.
	usart_disable(USART2);
    debug_println("]] uart end"); debug_flush();
}

int UARTInterface::available() { 
    //  Return the number of bytes to be read from the UART port.
    return responseQueue.read_available();
}

int UARTInterface::read() { 
    //  Return the next byte read from the UART port. Or return -1 if none.
    //  debug_println("uart_read");
    uint8_t ch;
    if (responseQueue.pop(ch)) { return ch; }
    return -1;  //  Nothing in response queue.
}

void UARTInterface::write(uint8_t ch) {
    //  Send the byte to the UART port.
    //  debug_println("uart_write"); debug_flush();
    usart_send(USART2, ch);
}

#endif  //  !SIMULATE_WISOL

#ifdef SIMULATE_WISOL //  If we are simulating a Wisol Sigfox module connected to the UART port...
//  The code below simulates a Wisol Sigfox module.  When we receive a Wisol AT command through the UART port,
//  we respond to the sender with a hardcoded ASCII response that will be returned after a fixed delay
//  (which is determined by the type of AT command).  When we receive the request downlink command,
//  we will wait 1 minute before returning the response, to simulate the delay for an actual Sigfox downlink.

#include "../../sigfox.h"  //  For COMMAND_TIMEOUT, UPLINK_TIMEOUT, DOWNLINK_TIMEOUT

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

UARTInterface::UARTInterface(unsigned rx, unsigned tx) {
    //  TODO: Init the UART port connected to the receive/transmit pins.
}

void UARTInterface::listen() {
    //  TODO: Listen to incoming data from the UART port.
}

void UARTInterface::end() {
    //  TODO: Close the UART port.
}

#endif  // SIMULATE_WISOL

