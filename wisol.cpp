//  Functions to send and receive messages to/from Sigfox network via 
//  Wisol modules WSSFM10R1AT, WSSFM10R2AT, WSSFM10R3AT and WSSFM10R4AT
//  for Sigfox zones RCZ1, RCZ2, RCZ3 and RCZ4 respectively.
#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "sensor.h"
#include "aggregate.h"
#include "downlink.h"
#include "wisol.h"

#include "radio.h"


static Sem_t sendSemaphore;

static SensorMsg sensorMsg;   //  Incoming sensor data message.
static SensorMsg periodicMsg; // Sent periodically to itself to trigger a radio transmission
static RadioMsg  radioMsg;    //  Outgoing radio msg with aggregated sensor readings in the payload


//  Send a message to radio every 20,000 milliseconds = 20 seconds.
#define SEND_INTERVAL ((unsigned long) 20 * 1000)


void network_task(void) {
  //  Loop forever, receiving sensor data messages and sending to UART Task to transmit to the network.
  //  Note: Declare task variables here before the task but don't populate them here
  //  unless they are not supposed to change. 
  //  Because the coroutine will execute this code repeatedly and repopulate the values.
  NetworkContext *context;

  task_open();
  periodicMsg.super.signal = TRANSMIT_SIG;
  msg_post_every(os_get_running_tid(), periodicMsg, SEND_INTERVAL);

  context = (NetworkContext *) task_get_data();

  for (;;) {

    // Wait for message: it could be a sensor message or the periodic Send message
    msg_receive(os_get_running_tid(), &sensorMsg);

    context = (NetworkContext *) task_get_data();

    if (sensorMsg.super.signal == TRANSMIT_SIG) {
      // fetch latest sensor data
      aggregate_getSensorData(&radioMsg.sensorData[0]);

      radioMsg.sendData = &radioMsg.sensorData[0];
      // send it to the radio
      msg_post_async(context->radioTaskID, radioMsg);
    }
    else if (sensorMsg.super.signal == SENSOR_DATA_SIG) {
      // save the received sensor data
      aggregate_saveSensorData(&sensorMsg);
    }
  }

  task_close();  //  End of the task. Should not come here.
}


/* Downlink Server Support: https://backend.sigfox.com/apidocs/callback
When a message needs to be acknowledged, the callback selected for the downlink data must 
send data in the response. It must contain the 8 bytes data that will be sent to the device 
asking for acknowledgment. The data is json formatted, and must be structured as the following :
  {"YOUR_DEVICE_ID" : { "downlinkData" : "deadbeefcafebabe"}}    
With YOUR_DEVICE_ID being replaced by the corresponding device id, in hexadecimal format, up to 8 digits. 
The downlink data must be 8 bytes in hexadecimal format.  For example:
  {"002C2EA1" : { "downlinkData" : "0102030405060708"}} */






static const char *cmdData;



void setup_wisol(
  NetworkContext *context,
  RadioContext *radioContext,
  int8_t radioTaskID,
  Country country0, 
  bool useEmulator0) {
  //  Init the Wisol context.

  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  sendSemaphore = sem_counting_create( maxCount, initValue );

  context->radioContext = radioContext;
  context->radioTaskID = radioTaskID;
  context->country = country0;
  context->useEmulator = useEmulator0;
  context->device[0] = 0;  //  Clear the device ID.
  context->pac[0] = 0;  //  Clear the PAC code.
  context->zone = context->country & RCZ_MASK;  //  Extract the zone from country node.
  context->lastSend = millis() + SEND_INTERVAL + SEND_INTERVAL;  //  Init the last send time to a high number so that sensor data will wait for Begin Step to complete.
  context->pendingResponse = false;
}

