//  Implements the temperature sensor for BME280. Caller must use a semaphore to
//  prevent concurrent access to BME280 module, which runs on a single I2C Bus.
#include <Arduino.h>
#include <EnvironmentCalculations.h>
#include <BME280I2C.h>
#include <Wire.h>
#include <stdlib.h>
#include <string.h>
#include <cocoos-cpp.h>  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"
#include "temp_sensor.h"

static uint8_t poll_sensor(void);
static uint8_t receive_sensor_data(float *data, uint8_t size);
static void init_sensor(uint8_t id, Evt_t *event, uint16_t period_ms);
static void next_channel(void);
static void prev_channel(void);

static SensorInfo sensor_info = {
  "tmp",  //  Name of sensor. The Structured Message field will use this name.
  &poll_sensor,  //  Function for polling sensor data.
  &receive_sensor_data,  //  Function for receiving sensor data.
};

static SensorControl sensor_control = {
  &init_sensor,
  &next_channel,
  &prev_channel
};

static Sensor sensor = {
  sensor_info,
  sensor_control
};

static float sensorData = NAN;  //  Default the sensor data to "Not A Number".
static uint8_t newData = 0;  //  Set to non-zero if there is new sensor data to be received.

static void init_sensor(uint8_t id, Evt_t *event, uint16_t poll_interval) {
  sensor.info.id = id;
  sensor.info.event = event;
  sensor.info.poll_interval = poll_interval;
}

static uint8_t poll_sensor(void) {
  //  Poll the sensor for new data. Return size of new data if new data available, 0 otherwise.
  debug("temp.poll_sensor"); ////

  //  Read sensor data from BME280.
  extern BME280I2C bme;  ////  TODO
  BME280::TempUnit tempUnit(BME280::TempUnit_Celsius);
  sensorData = bme.temp(tempUnit);  //  Get temperature in Celsius.

  //  Simulated sensor.
  //// sensorData = 12.3 + rand() % 10;

  newData = 1;  //  New sensor data now available.
  return newData;  //  Data should always be available.
}

static uint8_t receive_sensor_data(float *data, uint8_t size) {
  //  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.
  //// debug("temp.receive_sensor_data"); ////
  if (size >= 1) data[0] = sensorData;
  newData = 0;  //  Indicate that there is no new sensor data.
  return 1;  //  Only 1 float returned.
}

static void next_channel(void) {
}

static void prev_channel(void) {
}

Sensor *get_temp_sensor(void) {
  return &sensor;
}
