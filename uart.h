#ifndef UART_H_
#define UART_H_

#include <stddef.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.
#define maxUARTMsgLength 40  //  Max message length, e.g. 30 chars for AT$SF=0102030405060708090a0b0c
#define uartMsgPoolSize 2  //  Should not allow concurrent UART messages.  Hangs if <2.

//  UART Task accepts messages of this format.
//  TODO fix sendData
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
  size_t sendIndex;  //  Index of next char to be sent.
  unsigned long sentTime;  //  Timestamp at which we completed sending.
  char response[maxUARTMsgLength + 1];  //  Received response.
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
