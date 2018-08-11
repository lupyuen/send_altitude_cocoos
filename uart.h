#ifndef UART_H_
#define UART_H_

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.

//  UART Task accepts messages of this format.
struct UARTMsg {
  Msg_t super;  //  Required for all cocoOS messages.
  String buffer;
  unsigned long timeout;
  char markerChar;
  uint8_t expectedMarkerCount;
};

struct UARTContext {
  UARTMsg *msg;
  bool status;  //  Return status.
  int sendIndex;  //  Index of next char to be sent.
  unsigned long sentTime;  //  Timestamp at which we completed sending.
  String response;
  uint8_t actualMarkerCount;
};

void uart_task(void);
void setup_uart(UARTContext *uartContext, uint8_t rx, uint8_t tx, bool echo);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  UART_H_
