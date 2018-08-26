#ifndef RADIO_H_
#define RADIO_H_

#include <stddef.h>
#include <cocoos.h>

#include "radioDevice.h"

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.
#define RADIO_MSG_POOL_SIZE 2  //  Should not allow concurrent UART messages.  Hangs if <2.

struct SensorMsg;  //  Forward declaration

//  Radio Task accepts messages of this format for sending data.
struct RadioMsg {
  Msg_t super;                  //  Required for all cocoOS messages.
  const char *sendData;         //  Pointer to the string to be sent.
  unsigned long timeout;        //  Send timeout in milliseconds.
  char markerChar;              //  End-of-command marker character that we should count e.g. '\r'
  uint8_t expectedMarkerCount;  //  Wait for this number of markers until timeout.
  Evt_t successEvent;           //  Event to be triggered upon success.
  Evt_t failureEvent;           //  Event to be triggered upon failure.
  SensorMsg *responseMsg;       //  If not NULL, then send this response message when the response is completed.
  uint8_t responseTaskID;       //  Send to this task ID.
  char sensorData[MAX_SENSOR_DATA_SIZE25];

};

//  Radio Task maintains this context in the task data.
struct RadioContext {
  bool status;                  //  Return status.  True if successfully sent.
  unsigned long sentTime;       //  Timestamp at which we completed sending.
  char *response;               //  Pointer to the buffer for writing received response.
  RadioMsg *msg;                //  Message being sent. Set by radio_task() upon receiving a message.
  RadioDevice *radio;           //  Radio device in use
  Evt_t rxDoneEvent;            //  Event to be signalled by the radio device when upon message reception done
  NetworkCmd *cmd;              //  Command in progress
  unsigned nCommands;           //  Number of commands to send
  char device[MAX_DEVICE_ID_SIZE + 1];  //  Sigfox device ID read from device e.g. 002C2EA1
  char pac[MAX_DEVICE_CODE_SIZE + 1];  //  Sigfox PAC code read from device e.g. 5BEB8CF64E869BD1
  const char *downlinkData;  //  If downlink was requested, set the downlink hex string e.g. 0102030405060708.
  bool initialized;
};



void radio_task(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  RADIO_H_
