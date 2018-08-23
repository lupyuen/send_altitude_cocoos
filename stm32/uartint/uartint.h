//  UART Interface for STM32 UART port. Compatible with Arduino's SoftwareSerial.
#ifndef UARTINT_H_
#define UARTINT_H_
#ifdef __cplusplus  //  UARTInterface class for C++ only
#include <stdint.h>  //  For uint8_t

class UARTInterface {
public:
  UARTInterface(unsigned rx, unsigned tx);
  void begin(uint16_t bps);
  void listen();
  bool available();
  int read();
  void write(uint8_t ch);
  void end();
};

#endif  //  __cplusplus
#endif  //  UARTINT_H_
