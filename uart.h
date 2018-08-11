#ifndef UART_H_
#define UART_H_

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.

//  UART Task accepts messages of this format.
struct UARTMsg {
  Msg_t super;  //  Required for all cocoOS messages.
  String buffer;  //  String to be sent.
  unsigned long timeout;  //  Send timeout in milliseconds.
  char markerChar;  //  End-of-command marker character that we should count e.g. '\r'
  uint8_t expectedMarkerCount;  //  Wait for this number of markers until timeout.
  Evt_t successEvent;  //  Event to be triggered upon success.
  Evt_t failureEvent;  //  Event to be triggered upon failure.
};

//  UART Task maintains this context in the task data.
struct UARTContext {
  UARTMsg *msg;  //  Message being sent.
  bool status;  //  Return status.  True if successfully sent.
  int sendIndex;  //  Index of next char to be sent.
  unsigned long sentTime;  //  Timestamp at which we completed sending.
  String response;  //  Received response.
  uint8_t actualMarkerCount;  //  Actual number of markers received.
  unsigned long testTimer;  //  For testing timer.
};

void uart_task(void);
void setup_uart(UARTContext *uartContext, uint8_t rx, uint8_t tx, bool echo);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  UART_H_
