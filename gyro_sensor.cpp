//  Implements a simulated Gyroscope Sensor with 3 sensor values (x, y, z).
//  Demonstrates how we may return multiple float values as sensor data.
#include <Arduino.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"
#include "gyro_sensor.h"

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(void);
static uint8_t receive_sensor_data(float *data, uint8_t size);

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "gyr",                //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,         //  Function for initialising the sensor.
  &poll_sensor,         //  Function for polling sensor data.
  &receive_sensor_data  //  Function for receiving sensor data.
);

#define sensorDataSize 3  //  Our sensor data will have 3 floats.
static float sensorDataArray[sensorDataSize];  //  Array of floats for remembering the sensor data.

static SensorContext sensorContext;  //  Remembers the sensor context.
static uint8_t newDataSize = 0;  //  Number of new sensor data floats to be received.

static void init_sensor(void) {
  //  Initialise the sensor if necessary. sensor and sensorContext objects have been populated.
}

static uint8_t poll_sensor(void) {
  //  Poll the sensor for new data. Return size of new data if new data available, 0 otherwise.
  debug(sensor.info.name, " >> poll_sensor"); ////

  //  Simulated sensor data with random numbers.
  sensorDataArray[0] = (rand() % 500) / 10.0;
  sensorDataArray[1] = (rand() % 500) / 10.0;
  sensorDataArray[2] = (rand() % 500) / 10.0;

  newDataSize = sensorDataSize;  //  Return the size of sensor data.
  return newDataSize;  //  Data should always be available.
}

static uint8_t receive_sensor_data(float *data, uint8_t size) {
  //  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.
  //// debug("temp.receive_sensor_data"); ////
  uint8_t i;
  //  Copy the floats safely: Don't exceed the array size provided by caller.
  //  Also don't exceed the number of new data items.
  for (i = 0; i < size && i < newDataSize; i++) {
    data[i] = sensorDataArray[i];
  }
  newDataSize = 0;  //  Indicate that there is no new sensor data.
  return i;  //  Return the number of floats copied.
}

SensorContext *setup_gyro_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}
