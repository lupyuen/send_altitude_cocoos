#ifndef WISOL_H_
#define WISOL_H_

#include "platform.h"
#include "sigfox.h"
#include <cocoos.h>

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define NETWORK_MSG_POOL_SIZE 3  //  Allow up to 3 sensor data messages to be queued for the Network Task. Should be same as number of sensors.
#define MAX_NETWORK_CMD_LIST_SIZE 5  //  Allow up to 4 UART commands to be sent in a single Network Step.

struct RadioContext;  //  Forward declaration.

//  Defines a Wisol AT command string, to be sent via UART Task. Sequence is
//    sendData + payload + sendData2
struct NetworkCmd {
  // Command string to be sent
  const __FlashStringHelper *sendData;

  // Function to process the response, NULL if none.
  bool (*processFunc)(RadioContext *context, const char *response);

  const char *payload;  //  Additional payload to be sent right after sendData. Note: This is a pointer, not a buffer.

  // Second command string to be sent
  const __FlashStringHelper *sendData2;

  // Wait for this number of markers until timeout.
  uint8_t expectedMarkerCount;
};



extern NetworkCmd endOfList; //  Command to indicate end of command list.



#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  WISOL_H_
