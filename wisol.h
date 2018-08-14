#ifndef WISOL_H_
#define WISOL_H_

#include "platform.h"
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "uart.h"  //  For UARTContext
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define TEST_DOWNLINK false  //  Test Uplink only
// #define TEST_DOWNLINK true  //  Test Uplink and downlink

#define wisolMsgPoolSize 4  //  Allow up to 4 sensor data messages to be queued for the Wisol Task. Should be same as number of sensors (4).
#define maxWisolCmdListSize 5  //  Allow up to 5 UART commands to be sent in a single Wisol message.
#define maxSigfoxDeviceSize 8  //  Max number of chars in Sigfox device name.
#define maxSigfoxPACSize 16  //  Max number of chars in Sigfox PAC code.
#define beginSensorName "000"  //  If sensor name is this, then this is the "begin" message sent at startup.

const unsigned long SEND_DELAY = (unsigned long) 10 * 60 * 1000;  //  According to regulation, messages should be sent only every 10 minutes.
const unsigned int MAX_BYTES_PER_MESSAGE = 12;  //  Sigfox supports up to 12 bytes per message.
const unsigned long COMMAND_TIMEOUT = 60000;  //  Wait up to 60 seconds for response from Sigfox module.  Includes downlink response.

//  Define the countries that are supported.
enum Country {
  COUNTRY_AU = 'A'+('U' << 8),  //  Australia: RCZ4
  COUNTRY_BR = 'B'+('R' << 8),  //  Brazil: RCZ4
  COUNTRY_FR = 'F'+('R' << 8),  //  France: RCZ1
  COUNTRY_JP = 'J'+('P' << 8),  //  Japan: RCZ3
  COUNTRY_OM = 'O'+('M' << 8),  //  Oman: RCZ1
  COUNTRY_NZ = 'N'+('Z' << 8),  //  New Zealand: RCZ4
  COUNTRY_SA = 'S'+('A' << 8),  //  South Africa: RCZ1
  COUNTRY_SG = 'S'+('G' << 8),  //  Singapore: RCZ4
  COUNTRY_US = 'U'+('S' << 8),  //  USA: RCZ2
  COUNTRY_TW = 'T'+('W' << 8),  //  Taiwan: RCZ4
};

struct SensorMsg;  //  Forward declaration.
struct WisolContext;  //  Forward declaration.

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

  char device[maxSigfoxDeviceSize];  //  Sigfox device ID read from device e.g. 002C2EA1
  char pac[maxSigfoxPACSize];  //  Sigfox PAC code read from device e.g. 5BEB8CF64E869BD1
  bool firstTime;  //  Set by setup_wisol() to true if this is the first run.  
  bool status;  //  Return status.  True if command was successful.
  SensorMsg *msg;  //  Sensor data being sent. Set by wisol_task() upon receiving a message.

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
void getCmdBegin(WisolContext *context, WisolCmd list[]);
void getCmdSend(
  WisolContext *context, 
  WisolCmd list[], 
  const char *payload,
  bool enableDownlink);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  WISOL_H_
