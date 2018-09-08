#ifndef NETWORK_H_
#define NETWORK_H_

#include <cocoos.h>
#include "platform.h"
#include "sigfox.h"

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600   //  Connect to modem at this bps.
#define RADIO_MSG_POOL_SIZE 5        //  Should not allow concurrent UART messages.  Hangs if <2.
#define NETWORK_MSG_POOL_SIZE     5  //  Allow up to 5 sensor data messages to be queued for the Network Task. Should be same as number of sensors.
#define MAX_NETWORK_CMD_LIST_SIZE 5  //  Allow up to 5 UART commands to be sent in a single Network Step.


//  Radio Task accepts messages of this format for sending data.
struct NetworkMsg {
  Msg_t super;                  //  Required for all cocoOS messages.
  float sensorData[SENSOR_DATA_SIZE * N_SENSORS]; // Collection of sensor readings
};


struct NetworkContext;
class Radio;

struct NetworkCmd {
  // Command string to be sent
  const __FlashStringHelper *sendData;

  // Function to process the response, NULL if none.
  bool (*processFunc)(NetworkContext *context, const char *response);

  const char *payload;  //  Additional payload to be sent right after sendData. Note: This is a pointer, not a buffer.

  // Second command string to be sent
  const __FlashStringHelper *sendData2;

  // Wait for this number of markers until timeout.
  uint8_t expectedMarkerCount;
};

extern NetworkCmd endOfList; //  Command to indicate end of command list.



//  Radio Task maintains this context in the task data.
struct NetworkContext {
  bool status;                  //  Return status.  True if successfully sent.
  char *response;               //  Pointer to the buffer for writing received response.
  Radio *radio;           //  Radio device in use
  Evt_t rxDoneEvent;            //  Event to be signalled by the radio device when upon message reception done
  NetworkCmd *cmd;              //  Command in progress
  unsigned nCommands;           //  Number of commands to send
  char device[MAX_DEVICE_ID_SIZE + 1];  //  Sigfox device ID read from device e.g. 002C2EA1
  char pac[MAX_DEVICE_CODE_SIZE + 1];  //  Sigfox PAC code read from device e.g. 5BEB8CF64E869BD1
  const char *downlinkData;  //  If downlink was requested, set the downlink hex string e.g. 0102030405060708.
  bool initialized;
  int zone;  //  1 to 4 representing SIGFOX frequencies RCZ 1 to 4.
};


uint8_t network_setup(Radio *radio);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  NETWORK_H_
