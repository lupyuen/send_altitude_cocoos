#ifndef UART_H_
#define UART_H_

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.

struct UARTContext {
};

void uart_task(void);
void setup_uart(uint8_t rx, uint8_t tx, bool echo);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  UART_H_
