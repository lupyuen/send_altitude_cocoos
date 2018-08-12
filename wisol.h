#ifndef WISOL_H_
#define WISOL_H_

#ifdef ARDUINO
#include <Arduino.h>
#endif  //  ARDUINO
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "uart.h"  //  For UARTContext
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define wisolMsgPoolSize 2  //  Allow up to 5 outgoing sensor messages to be queued.
#define maxWisolCmdListSize 6  //  Allow up to 6 UART commands to be sent in a single Wisol message.
#define maxSigfoxDeviceSize 10  //  Max number of chars in Sigfox device name.
#define maxSigfoxPACSize 20  //  Max number of chars in Sigfox PAC code.

//  According to regulation, messages should be sent only every 10 minutes.
const unsigned long SEND_DELAY = (unsigned long) 10 * 60 * 1000;
const unsigned int MAX_BYTES_PER_MESSAGE = 12;  //  Only 12 bytes per message.
const unsigned int COMMAND_TIMEOUT = 1000;  //  Wait up to 1 second for response from SIGFOX module.

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

//  Wisol Task accepts messages containing sensor data, in this format.
struct WisolMsg {
  Msg_t super;  //  Required for all cocoOS messages.
};

struct WisolContext;

//  Defines a Wisol AT command string, to be sent via UART Task.
struct WisolCmd {
  const __FlashStringHelper *sendData;  //  String to be sent, in F() flash memory. 
  uint8_t expectedMarkerCount;  //  Wait for this number of markers until timeout.
  bool (*processFunc)(WisolContext *context, const char *response);  //  Function to process the response, NULL if none.  
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
  WisolMsg *msg;  //  Sensor data being sent. Set by wisol_task() upon receiving a message.

  WisolCmd *cmdList;  //  List of Wisol AT commands being sent.
  int cmdIndex;  //  Index of cmdList being sent.
};

void setup_wisol(
  WisolContext *context, 
  UARTContext *uartContext, 
  int8_t uartTaskID, 
  Country country0, 
  bool useEmulator0);
void wisol_task(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  WISOL_H_
