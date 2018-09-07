//  UART Interface for STM32 UART port. Compatible with Arduino's SoftwareSerial.
//  We support only Blue Pill USART Port 2:
//  RX2 = Pin PA3
//  TX2 = Pin PA2
#ifndef UARTINT_H_
#define UARTINT_H_
#ifdef __cplusplus  //  UARTInterface class for C++ only
#include <stdint.h>  //  For uint8_t

class UARTInterface {
public:
  UARTInterface(unsigned rx, unsigned tx);  //  Init the UART interface.  rx and tx are not used for STM32.
  void begin(uint16_t bps);  //  Open the UART port.
  void listen();  //  Start receiving incoming data from the UART port.  Set up the receive interrupt.
  int available();  //  Return the number of bytes to be read from the UART port.
  int read();  //  Return the next byte read from the UART port. Or return -1 if none.
  void write(uint8_t ch);  //  Send the byte to the UART port.
  void end();  //  Close the UART port.  Disable interrupts.
};

#endif  //  __cplusplus
#endif  //  UARTINT_H_
