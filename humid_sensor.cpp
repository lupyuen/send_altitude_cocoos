//  Implements the humidity sensor for BME280. Caller must use a semaphore to
//  prevent concurrent access to BME280 module, which runs on a single I2C Bus.
#include <Arduino.h>
#include <EnvironmentCalculations.h>
#include <BME280I2C.h>
#include <Wire.h>
#include <stdlib.h>
#include <string.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"
#include "bme280.h"
#include "humid_sensor.h"

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(void);
static uint8_t receive_sensor_data(float *data, uint8_t size);
static void next_channel(void);
static void prev_channel(void);

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "hmd",  //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,  //  Function for initialising the sensor.
  &poll_sensor,  //  Function for polling sensor data.
  &receive_sensor_data,  //  Function for receiving sensor data.
  &next_channel,  //  Not used.
  &prev_channel  //  Not used.
);

static SensorContext sensorContext;  //  Remembers the sensor context.
static float sensorData = NAN;  //  Default the sensor data to "Not A Number".
static uint8_t newDataSize = 0;  //  Set to non-zero if there is new sensor data to be received.

static void init_sensor(void) {
  //  Initialise the sensor if necessary. sensor and sensorContext objects have been populated.
}

static uint8_t poll_sensor(void) {
  //  Poll the sensor for new data. Return size of new data if new data available, 0 otherwise.
  debug(sensor.info.name, " >> poll_sensor"); ////
  
  //  Read sensor data from BME280.
  sensorData = bme.hum();  //  Get relative humidity as percentage.

  //  Simulated sensor.
  //// sensorData = 12.3 + rand() % 10;

  newDataSize = 1;  //  New sensor data now available, size is 1 float.
  return newDataSize;  //  Data should always be available.
}

static uint8_t receive_sensor_data(float *data, uint8_t size) {
  //  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.
  //// debug("temp.receive_sensor_data"); ////
  if (size >= 1) data[0] = sensorData;
  newDataSize = 0;  //  Indicate that there is no new sensor data.
  return 1;  //  Only 1 float returned.
}

SensorContext *setup_humid_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

//  Not used.
static void next_channel(void) {}
static void prev_channel(void) {}