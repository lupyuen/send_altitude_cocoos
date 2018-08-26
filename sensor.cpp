//  Common code for all sensors.
//#define DISABLE_DEBUG_LOG  //  Disable debug logging.
#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "sensor.h"
#include "display.h"

static uint8_t nextSensorID = 1;  //  Next sensor ID to be allocated.  Running sequence number.

void setup_sensor_context( SensorContext *context, Sensor *sensor, uint16_t pollInterval, uint8_t taskID) {
  //  Set up the sensor context and call the sensor to initialise itself.
  //  Allocate a unique sensor ID and create the event.
  uint8_t sensorID =  nextSensorID++;
  Evt_t event = event_create();

  //  Initialise the sensor values.
  sensor->info.id = sensorID; 
  sensor->info.event = &event;
  sensor->info.poll_interval = pollInterval;

  //  Set the context.
  context->sensor = sensor;
  context->receive_task_id = taskID;

  //  Call the sensor to do initialisation.
  sensor->control.init_sensor_func();
}

void sensor_task(void) {
  //  Background task to receive and process sensor data.
  //  This task will be reused by all sensors: temperature, humidity, altitude.
  //  Don't declare any static variables inside here because they will conflict
  //  with other sensors.
  SensorContext *context = NULL;  //  Declared outside the task to prevent cross-initialisation error in C++.

  task_open();  //  Start of the task. Must be matched with task_close().
  context = (SensorContext *) task_get_data();

  // Initialize the SensorMsg contained in this sensor context
  context->msg.super.signal = SENSOR_DATA_SIG;
  context->msg.sensorId = context->sensor->info.id;

  for (;;) {

    sem_wait(i2cSemaphore);

    //  We have to fetch the context pointer again after the wait.
    context = (SensorContext *) task_get_data();

    //  Poll for the sensor data and copy into the display message.
    context->msg.count = context->sensor->info.poll_sensor_func(context->msg.data, MAX_SENSOR_DATA_SIZE);

    //  We are done with the I2C Bus.  Release the semaphore so that another task can fetch the sensor data.
    sem_signal(i2cSemaphore);

    context = (SensorContext *) task_get_data();

    //  Do we have new data?
    if (context->msg.count > 0) {
      msg_post_async(context->receive_task_id, context->msg);
    }

    //  Wait a short while before polling the sensor again.
    task_wait(context->sensor->info.poll_interval);

    context = (SensorContext *) task_get_data();
  }

  task_close();  //  End of the task. Should never come here.
}

uint8_t receive_sensor_data(float *dest, uint8_t destSize, const float *src, uint8_t srcSize ) {
  //  Copy the received sensor data array into the provided data buffer.
  //  Return the number of floats copied.
  uint8_t i;
  //  Copy the floats safely: Don't exceed the array size provided by caller.
  //  Also don't exceed the number of available sensor data items.
  for (i = 0; i < destSize && i < srcSize && i < MAX_SENSOR_DATA_SIZE; i++) {
    dest[i] = src[i];
  }
  return i;  //  Return the number of floats copied.
}

//  SensorInfo constructor for C++ only.
SensorInfo::SensorInfo(
  const char name0[],
  uint8_t (*poll_sensor_func0)(float *data, uint8_t size)
) {
  name = name0;
  poll_sensor_func = poll_sensor_func0;
}

//  SensorInfo constructor for C++ only.
SensorControl::SensorControl(
  void (*init_sensor_func0)(void),
  void (*next_channel_func0)(void),
  void (*prev_channel_func0)(void)
) {
  init_sensor_func = init_sensor_func0;
  next_channel_func = next_channel_func0;
  prev_channel_func = prev_channel_func0;
}

//  Sensor constructor for C++ only.
Sensor::Sensor(
  const char name[],
  void (*init_sensor_func)(void),
  uint8_t (*poll_sensor_func)(float *data, uint8_t size),
  void (*next_channel_func)(void),
  void (*prev_channel_func)(void)
): 
  info(name, poll_sensor_func),
  control(init_sensor_func, next_channel_func, prev_channel_func) {
}

