#ifndef UART_H_
#define UART_H_

#include <stddef.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.
#define UART_MSG_POOL_SIZE 2  //  Should not allow concurrent UART messages.  Hangs if <2.

struct SensorMsg;  //  Forward declaration

//  UART Task accepts messages of this format for sending data.
struct UARTMsg {
  Msg_t super;  //  Required for all cocoOS messages.
  const char *sendData;  //  Pointer to the string to be sent.
  unsigned long timeout;  //  Send timeout in milliseconds.
  char markerChar;  //  End-of-command marker character that we should count e.g. '\r'
  uint8_t expectedMarkerCount;  //  Wait for this number of markers until timeout.
  Evt_t successEvent;  //  Event to be triggered upon success.
  Evt_t failureEvent;  //  Event to be triggered upon failure.
  SensorMsg *responseMsg;  //  If not NULL, then send this response message when the response is completed.
  uint8_t responseTaskID;  //  Send to this task ID.
};

//  UART Task maintains this context in the task data.
struct UARTContext {
  bool status;  //  Return status.  True if successfully sent.
  size_t sendIndex;  //  Index of next char to be sent.
  unsigned long sentTime;  //  Timestamp at which we completed sending.
  char *response;  //  Pointer to the buffer for writing received response.
  uint8_t actualMarkerCount;  //  Actual number of markers received.
  unsigned long testTimer;  //  For testing timer.
  UARTMsg *msg;  //  Message being sent. Set by uart_task() upon receiving a message.
};

void setup_uart(
  UARTContext *context, 
  char *response, 
  uint8_t rx, 
  uint8_t tx, 
  bool echo);
void uart_task(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  UART_H_
