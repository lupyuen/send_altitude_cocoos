// Aggregate sensor data
// Receives sensor messages containing sensor readings and saves them in an array.
// The sensor readings are periodically put together in a message sent to radio task.

#include <string.h>
#include "sensor.h"
#include "aggregate.h"
#include "sigfox.h"
#include "radio.h"
#include "platform.h"

#define NO_SENSOR 0xff

static void createRadioMessage(RadioMsg *msg);
static void save(const SensorMsg *msg);
static SensorMsg *recallSensor(uint8_t id);
static void copySensorData(SensorMsg *dest, const SensorMsg *src);
static void addToPayload(char* payload, int data);

//  Remember the last sensor value of each sensor.
static SensorMsg sensorData[N_SENSORS];


void setup_aggregate() {

  //  Clear the aggregated sensor data.
  for (int i = 0; i < N_SENSORS; i++) {
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

  // Setup a message sent to the task itself to trigger radio transmission
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

static void createRadioMessage(RadioMsg *msg) {
  float *payload = &msg->sensorData[0];

  payload[0] = 0;

  for (int i = 0; i < N_SENSORS; i++) {

    //  Get each sensor data and add to the message payload.
    if (sensorData[i].sensorId != NO_SENSOR) {
      for (int j = 0; j < SENSOR_DATA_SIZE; j++) {
      *payload++ = sensorData[i].data[j];
      }
    }
  }
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
    for (int i = 0; i < N_SENSORS; i++) {
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


