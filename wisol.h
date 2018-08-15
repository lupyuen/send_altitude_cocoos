#ifndef WISOL_H_
#define WISOL_H_

#include "platform.h"
#include "sigfox.h"
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define WISOL_MSG_POOL_SIZE 4  //  Allow up to 4 sensor data messages to be queued for the Wisol Task. Should be same as number of sensors (4).
#define MAX_WISOL_CMD_LIST_SIZE 5  //  Allow up to 4 UART commands to be sent in a single Wisol message.
#define BEGIN_SENSOR_NAME "000"  //  If sensor name is this, then this is the "begin" message sent at startup.

struct SensorMsg;  //  Forward declaration.
struct WisolContext;  //  Forward declaration.
struct UARTContext;  //  Forward declaration.

//  Defines a Wisol AT command string, to be sent via UART Task. Sequence is
//    sendData + payload + sendData2
struct WisolCmd {
  const __FlashStringHelper *sendData;  //  Command string to be sent, in F() flash memory. 
  uint8_t expectedMarkerCount;  //  Wait for this number of markers until timeout.
  bool (*processFunc)(WisolContext *context, const char *response);  //  Function to process the response, NULL if none.
  const char *payload;  //  Additional payload to be sent right after sendData. Note: This is a pointer, not a buffer.
  const __FlashStringHelper *sendData2;  //  Second command string to be sent, in F() flash memory. 
};

//  Wisol Task maintains this context in the task data.
struct WisolContext {
  UARTContext *uartContext;  //  Context of the UART Task.
  uint8_t uartTaskID;  //  Task ID of the UART Task.  Wisol Task transmits UART data by sending a message to this task. 
  int zone;  //  1 to 4 representing SIGFOX frequencies RCZ 1 to 4.
  Country country;   //  Country to be set for SIGFOX transmission frequencies.
  bool useEmulator;  //  Set to true if using SNEK Emulator.

  char device[MAX_DEVICE_ID_SIZE + 1];  //  Sigfox device ID read from device e.g. 002C2EA1
  char pac[MAX_DEVICE_CODE_SIZE + 1];  //  Sigfox PAC code read from device e.g. 5BEB8CF64E869BD1
  bool status;  //  Return status.  True if command was successful.
  SensorMsg *msg;  //  Sensor data being sent. Set by wisol_task() upon receiving a message.
  const char *downlinkData;  //  If downlink was requested, set the downlink hex string e.g. 0102030405060708.

  WisolCmd *cmdList;  //  List of Wisol AT commands being sent.
  int cmdIndex;  //  Index of cmdList being sent.
};

extern WisolCmd endOfList; //  Command to indicate end of command list.

void setup_wisol(
  WisolContext *context, 
  UARTContext *uartContext, 
  int8_t uartTaskID, 
  Country country0, 
  bool useEmulator0);
void wisol_task(void);
void getCmdBegin(
  WisolContext *context, 
  WisolCmd list[],
  int listSize);
void getCmdSend(
  WisolContext *context, 
  WisolCmd list[],
  int listSize, 
  const char *payload,
  bool enableDownlink);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  WISOL_H_
