//  Common code for all sensors.
#include <Arduino.h>
#include <string.h>
#include <stdio.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"
#include "display.h"

uint8_t nextSensorID = 1;  //  Next sensor ID to be allocated.  Running sequence number.

void setup_sensor_context(
  SensorContext *context,
  Sensor *sensor,
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
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
  context->display_task_id = displayTaskID;

  //  Call the sensor to do initialisation.
  sensor->control.init_sensor_func();
}

void sensor_task(void) {
  //  Background task to receive and process sensor data.
  //  This task will be reused by all sensors: temperature, humidity, altitude.
  //  Don't declare any static variables inside here because they will conflict
  //  with other sensors.
  SensorContext *context = NULL;  //  Declared outside the task to prevent cross-initialisation error in C++.
  uint8_t sensorDataCount;
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_post() in C++.
  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) {  //  Run the sensor processing code forever. So the task never ends.
    //  We should not make this variable static, because the task data should be unique for each task.
    context = (SensorContext *) task_get_data();

    //  This code is executed by multiple sensors. We use a global semaphore to prevent 
    //  concurrent access to the single shared I2C Bus on Arduino Uno.
    debug(context->sensor->info.name, F(" >> Wait for semaphore")); ////
    sem_wait(i2cSemaphore);  //  Wait until no other sensor is using the I2C Bus. Then lock the semaphore.
    debug(context->sensor->info.name, F(" >> Got semaphore")); ////

    //  We have to fetch the context pointer again after the wait.
    context = (SensorContext *) task_get_data();

    //  Prepare a display message for copying the sensor data.
    DisplayMsg msg;
    msg.super.signal = context->sensor->info.id;  //  e.g. TEMP_DATA, GYRO_DATA.
    memset(msg.name, 0, maxSensorNameSize + 1);  //  Zero the name array.
    strncpy(msg.name, context->sensor->info.name, maxSensorNameSize);  //  Set the sensor name e.g. tmp

    //  Poll for the sensor data and copy into the display message.
    msg.count = context->sensor->info.poll_sensor_func(msg.data, maxSensorDataSize);

    //  Do we have new data?
    if (msg.count > 0) {
      //  If we have new data, send the message. Note: When posting a message, its contents are cloned into the message queue.
      debug(msg.name, F(" >> Send msg")); ////
      msg_post(context->display_task_id, msg);
    }

    //  We are done with the I2C Bus.  Release the semaphore so that another task can fetch the sensor data.
    debug(context->sensor->info.name, F(" >> Release semaphore")); ////
    sem_signal(i2cSemaphore);

    //  Wait a short while before polling the sensor again.
    debug(context->sensor->info.name, F(" >> Wait interval")); ////
    task_wait(context->sensor->info.poll_interval);
  }
  debug(F("task_close")); ////
  task_close();  //  End of the task. Should never come here.
}

uint8_t receive_sensor_data(float *sensorDataArray, uint8_t sensorDataSize, float *data, uint8_t size) {
  //  Copy the received sensor data array into the provided data buffer.
  //  Return the number of floats copied.
  //// debug(F("receive_sensor_data")); ////
  uint8_t i;
  //  Copy the floats safely: Don't exceed the array size provided by caller.
  //  Also don't exceed the number of available sensor data items.
  for (i = 0; i < size && i < sensorDataSize && i < maxSensorDataSize; i++) {
    data[i] = sensorDataArray[i];
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
