// Aggregate sensor data
// Receives sensor messages containing sensor readings and saves them in an array.
// The sensor readings are periodically put together in a message sent to radio task.

#include <string.h>
#include "sensor.h"
#include "aggregate.h"
#include "sigfox.h"
#include "radio.h"
#include "platform.h"

#define PAYLOAD_SIZE (1 + MAX_MESSAGE_SIZE * 2)  //  Each byte takes 2 hex digits. Add 1 for terminating null.
#define NO_SENSOR 0xff

static void createRadioMessage(RadioMsg *msg);
static void save(const SensorMsg *msg);
static SensorMsg *recallSensor(uint8_t id);
static void copySensorData(SensorMsg *dest, const SensorMsg *src);
static void addToPayload(char* payload, int data);

//  Remember the last sensor value of each sensor.
static SensorMsg sensorData[MAX_SENSOR_COUNT];

//  Buffer for constructing the message payload to be sent, in hex digits, plus terminating null.
//static char payload[PAYLOAD_SIZE];  //  e.g. "0102030405060708090a0b0c"


//static SensorMsg periodicMsg; //  Sent periodically to itself to trigger a radio transmission
//static RadioMsg  radioMsg;    //  Outgoing radio msg with aggregated sensor readings in the payload

void setup_aggregate() {

  //  Clear the aggregated sensor data.
  for (int i = 0; i < MAX_SENSOR_COUNT; i++) {
      sensorData[i].sensorId = NO_SENSOR;
      sensorData[i].count = 0;  //  Clear the values.
  }
}

void aggregate_task(void) {
  //  Loop forever, receiving sensor data messages and sending to radio task to transmit to the network.
  //  Note: Declare task variables here before the task but don't populate them here
  //  unless they are not supposed to change.
  //  Because the coroutine will execute this code repeatedly and repopulate the values.
  AggregateContext *context;

  task_open();

  context = (AggregateContext *) task_get_data();
  context->periodicMsg.super.signal = TRANSMIT_SIG;

  msg_post_every(os_get_running_tid(), context->periodicMsg, context->sendPeriodInSeconds *  TICKS_PER_S);

  for (;;) {

    SensorMsg sensorMsg;

    // Wait for message: it could be a sensor message or the periodic Send message
    msg_receive(os_get_running_tid(), &sensorMsg);

    context = (AggregateContext *) task_get_data();

    if (sensorMsg.super.signal == TRANSMIT_SIG) {

      // create radio message from the saved sensor readings
      createRadioMessage(&context->radioMsg);

      // send it to the radio
      msg_post_async(context->radioTaskID, context->radioMsg);
    }
    else if (sensorMsg.super.signal == SENSOR_DATA_SIG) {
      // save the received sensor data
      save(&sensorMsg);
    }
  }

  task_close();  //  End of the task. Should not come here.
}

static void save(const SensorMsg *msg) {
  //  Aggregate the sensor data.  Here we just save the last value for each sensor.
  SensorMsg *savedSensor = recallSensor(msg->sensorId);

  if (savedSensor != NULL) {
    copySensorData(savedSensor, msg);
  }
}

//static void createRadioMessage(RadioMsg *msg) {
//  //  Create a new Sigfox message. Add a running sequence number to the message.
//  payload[0] = 0;
//  static int sequenceNumber = 0;
//
//  addToPayload(sequenceNumber++);
//
//  for (int i = 0; i < MAX_SENSOR_COUNT; i++) {
//
//    //  Get each sensor data and add to the message payload.
//    float data = 0;
//
//    if (sensorData[i].sensorId != NO_SENSOR) {
//      data = sensorData[i].data[0];
//      int scaledData = data * 10; //  Scale up by 10 to send 1 decimal place. So 27.1 becomes 271
//      addToPayload(scaledData);
//    }
//  }
//
//  //  If the payload has odd number of digits, pad with '0'.
//  int length = strlen(payload);
//  if (length % 2 != 0 && length < PAYLOAD_SIZE - 1) {
//      payload[length] = '0';
//      payload[length + 1] = 0;
//  }
//
//  // copy to output buffer
//  strcpy(&msg->sensorData[0], payload);
//}

static void createRadioMessage(RadioMsg *msg) {
  //  Create a new Sigfox message. Add a running sequence number to the message.
  char *payload = &msg->sensorData[0];

  payload[0] = 0;
  static int sequenceNumber = 0;

  addToPayload(payload, sequenceNumber++);

  for (int i = 0; i < MAX_SENSOR_COUNT; i++) {

    //  Get each sensor data and add to the message payload.
    float data = 0;

    if (sensorData[i].sensorId != NO_SENSOR) {
      data = sensorData[i].data[0];
      int scaledData = data * 10; //  Scale up by 10 to send 1 decimal place. So 27.1 becomes 271
      addToPayload(payload,scaledData);
    }
  }

  //  If the payload has odd number of digits, pad with '0'.
  int length = strlen(payload);
  if (length % 2 != 0 && length < PAYLOAD_SIZE - 1) {
      payload[length] = '0';
      payload[length + 1] = 0;
  }

  // copy to output buffer
  strcpy(&msg->sensorData[0], payload);
}

//static void addToPayload(int data) {
//    //  Add the integer data to the message payload as numDigits digits in hexadecimal.
//    //  So data=1234 and numDigits=4, it will be added as "1234".  Not efficient, but easy to read.
//    int length = strlen(payload);
//
//    if (length + 4 >= PAYLOAD_SIZE) {  //  No space for numDigits hex digits.
//        return;
//    }
//
//    for (int i = 4 - 1; i >= 0; i--) {  //  Add the digits in reverse order (right to left).
//        int d = data % 10;  //  Take the last digit.
//        data = data / 10;  //  Shift to the next digit.
//        payload[length + i] = '0' + d;  //  Write the digit to payload: 1 becomes '1'.
//    }
//    payload[length + 4] = 0;  //  Terminate the payload after adding numDigits hex chars.
//}

static void addToPayload(char* payload, int data) {
    //  Add the integer data to the message payload as numDigits digits in hexadecimal.
    //  So data=1234 and numDigits=4, it will be added as "1234".  Not efficient, but easy to read.
    int length = strlen(payload);

    if (length + 4 >= PAYLOAD_SIZE) {  //  No space for numDigits hex digits.
        return;
    }

    for (int i = 4 - 1; i >= 0; i--) {  //  Add the digits in reverse order (right to left).
        int d = data % 10;  //  Take the last digit.
        data = data / 10;  //  Shift to the next digit.
        payload[length + i] = '0' + d;  //  Write the digit to payload: 1 becomes '1'.
    }
    payload[length + 4] = 0;  //  Terminate the payload after adding numDigits hex chars.
}

static void copySensorData(SensorMsg *dest, const SensorMsg *src) {
    //  Copy sensor data from src to dest.
    for (int i = 0; i < src->count; i++) {
        dest->data[i] = src->data[i];
    }
    dest->count = src->count;
}

static SensorMsg *recallSensor(uint8_t id) {
    //  Return the sensor data for the sensor name.  If not found, allocate
    //  a new SensorMsg and return it.  If no more space, return NULL.
    int emptyIndex = -1;
    for (int i = 0; i < MAX_SENSOR_COUNT; i++) {
        //  Search for the sensor id in our data.
        if (sensorData[i].sensorId == id) {
            return &sensorData[i];  //  Found it.
        }

        //  Find the first empty element.
        if (emptyIndex == -1 && sensorData[i].sensorId == NO_SENSOR) {
            emptyIndex = i;
        }
    }

    //  Allocate a new element.
    if (emptyIndex == -1) {
      //  No more space.
      return NULL;
    }

    sensorData[emptyIndex].sensorId = id;
    sensorData[emptyIndex].count = 0;  //  Clear the values.
    sensorData[emptyIndex].data[0] = 0;  //  Reset to 0 in case we need to send.
    return &sensorData[emptyIndex];
}


