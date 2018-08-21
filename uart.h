//  UART Task for sending and receiving data to/from the UART port.
//  The UART port is connected to the Wisol Sigfox module.
#ifndef UART_H_
#define UART_H_
#include "platform.h"
#include <stddef.h>
#include <cocoos.h>
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

#define UART_TX_PORT 4  //  Transmit port for Wisol module.
#define UART_RX_PORT 5  //  Receive port for Wisol module.
#define UART_BITS_PER_SECOND 9600  //  Connect to Wisol module at this bps.
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
  UARTContext *context,  //  Will be used to store the context of the UART Task.
  char *response,        //  Buffer that will be used to store the UART response.
  bool echo);            //  If true, all commands will be echoed to console.
void uart_task(void);

END_EXTERN_C  //  End of extern C scope.
#endif  //  UART_H_
