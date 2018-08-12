#ifndef UART_H_
#define UART_H_

#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.
#define maxUARTMsgLength 30  //  Max message length. 12-byte message = 24 ASCII chars
#define uartMsgPoolSize 2  //  Should not have concurrent messages.

//  UART Task accepts messages of this format.
struct UARTMsg {
  Msg_t super;  //  Required for all cocoOS messages.
  char sendData[maxUARTMsgLength + 1];  //  String to be sent.  Must be a char array because messages are copied into the queue.
  unsigned long timeout;  //  Send timeout in milliseconds.
  char markerChar;  //  End-of-command marker character that we should count e.g. '\r'
  uint8_t expectedMarkerCount;  //  Wait for this number of markers until timeout.
  Evt_t successEvent;  //  Event to be triggered upon success.
  Evt_t failureEvent;  //  Event to be triggered upon failure.
};

//  UART Task maintains this context in the task data.
struct UARTContext {
  bool status;  //  Return status.  True if successfully sent.
  int sendIndex;  //  Index of next char to be sent.
  unsigned long sentTime;  //  Timestamp at which we completed sending.
  String response;  //  Received response.
  uint8_t actualMarkerCount;  //  Actual number of markers received.
  unsigned long testTimer;  //  For testing timer.
  UARTMsg *msg;  //  Message being sent. Set by uart_task() upon receiving a message.
};

void setup_uart(UARTContext *uartContext, uint8_t rx, uint8_t tx, bool echo);
void uart_task(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  UART_H_
