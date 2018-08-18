#ifndef WISOL_H_
#define WISOL_H_

#include "platform.h"
#include "sigfox.h"
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define NETWORK_MSG_POOL_SIZE MAX_SENSOR_COUNT  //  Allow up to 3 sensor data messages to be queued for the Network Task. Should be same as number of sensors.
#define MAX_NETWORK_CMD_LIST_SIZE 5  //  Allow up to 4 UART commands to be sent in a single Network Step.

struct SensorMsg;  //  Forward declaration.
struct NetworkContext;  //  Forward declaration.
struct UARTContext;  //  Forward declaration.

//  Defines a Wisol AT command string, to be sent via UART Task. Sequence is
//    sendData + payload + sendData2
struct NetworkCmd {
  const __FlashStringHelper *sendData;  //  Command string to be sent, in F() flash memory. 
  uint8_t expectedMarkerCount;  //  Wait for this number of markers until timeout.
  bool (*processFunc)(NetworkContext *context, const char *response);  //  Function to process the response, NULL if none.
  const char *payload;  //  Additional payload to be sent right after sendData. Note: This is a pointer, not a buffer.
  const __FlashStringHelper *sendData2;  //  Second command string to be sent, in F() flash memory. 
};

//  Network Task maintains this context in the task data.
struct NetworkContext {
  UARTContext *uartContext;  //  Context of the UART Task.
  uint8_t uartTaskID;  //  Task ID of the UART Task.  Network Task transmits UART data by sending a message to this task.
  int zone;  //  1 to 4 representing SIGFOX frequencies RCZ 1 to 4.
  Country country;   //  Country to be set for SIGFOX transmission frequencies.
  bool useEmulator;  //  Set to true if using SNEK Emulator.
  void (*stepBeginFunc)(  //  Begin Step: Return the Wisol AT Commands to be executed at startup.
    NetworkContext *context,
    NetworkCmd list[],
    int listSize);
  void (*stepSendFunc)(  //  Send Step: Return the Wisol AT Commands to be executed when sending a payload.
    NetworkContext *context,
    NetworkCmd list[],
    int listSize, 
    const char *payload,
    bool enableDownlink);

  char device[MAX_DEVICE_ID_SIZE + 1];  //  Sigfox device ID read from device e.g. 002C2EA1
  char pac[MAX_DEVICE_CODE_SIZE + 1];  //  Sigfox PAC code read from device e.g. 5BEB8CF64E869BD1
  bool status;  //  Return status.  True if command was successful.
  bool pendingResponse;  //  True if we are waiting for the send response to be processed.
  bool (*pendingProcessFunc)(NetworkContext *context, const char *response);  //  Function to process the pending response, NULL if none.
  unsigned long lastSend;  //  Timestamp of last sent message in milliseconds.  Used for throttling.
  SensorMsg *msg;  //  Sensor data being sent. Set by network_task() upon receiving a message.
  const char *downlinkData;  //  If downlink was requested, set the downlink hex string e.g. 0102030405060708.

  NetworkCmd *cmdList;  //  List of Wisol AT commands being sent.
  int cmdIndex;  //  Index of cmdList being sent.
};

extern NetworkCmd endOfList; //  Command to indicate end of command list.

void setup_wisol(
  NetworkContext *context,
  UARTContext *uartContext, 
  int8_t uartTaskID, 
  Country country0, 
  bool useEmulator0);
void network_task(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  WISOL_H_
