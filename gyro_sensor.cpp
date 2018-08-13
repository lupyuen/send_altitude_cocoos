//  Implements a simulated Gyroscope Sensor with 3 sensor values (x, y, z).
//  Demonstrates how we may return multiple float values as sensor data.
#include <Arduino.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "display.h"
#include "sensor.h"
#include "gyro_sensor.h"

#ifdef SENSOR_DATA

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(float *data, uint8_t size);

//  Number of floats that this sensor returns as sensor data.
#define sensorDataSize 3

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "gyr",                //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,         //  Function for initialising the sensor.
  &poll_sensor          //  Function for polling sensor data.
);

static SensorContext sensorContext;  //  Remembers the sensor context.
static float sensorDataArray[sensorDataSize];  //  Array of floats for remembering the sensor data.

static void init_sensor(void) {
  //  Initialise the sensor if necessary. sensor and sensorContext objects have been populated.
}

static uint8_t poll_sensor(float *data, uint8_t size) {
  //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  debug(sensor.info.name, F(" >> poll_sensor")); ////

  //  Simulated sensor data with random numbers.
  sensorDataArray[0] = (rand() % 500) / 10.0;
  sensorDataArray[1] = (rand() % 500) / 10.0;
  sensorDataArray[2] = (rand() % 500) / 10.0;

  //  Copy the received sensor data and return the number of floats copied.
  return receive_sensor_data(sensorDataArray, sensorDataSize, data, size);
}

SensorContext *setup_gyro_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

#endif  //  SENSOR_DATA
