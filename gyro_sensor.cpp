//  Implements a simulated Gyroscope Sensor with 3 sensor values (x, y, z).
#include <Arduino.h> ////
#include <Time.h> ////
#include <TimeLib.h> ////
typedef unsigned long time_t; //// TODO: Fix this declaration

#include "sensor.h"
#include "gyro_sensor.h"
#include <string.h>
#include <cocoos-cpp.h>  //  TODO: Workaround for cocoOS in C++
#include <stdio.h>
#include <stdlib.h>

static uint8_t poll_sensor(void);
static uint8_t receive_sensor_data(float *data, uint8_t size);
static void init_sensor(uint8_t id, Evt_t *event, uint16_t period_ms);
static void next_channel(void);

static Sensor sensor(
  "gyr",  //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,  //  Function for initialising the sensor.
  &poll_sensor,  //  Function for polling sensor data.
  &receive_sensor_data,  //  Function for receiving sensor data.
  &next_channel,  //  Not used.
  &next_channel  //  Not used.
);

static SensorContext sensorContext;  //  Remembers the sensor context.
static uint8_t newData = 0;

static uint8_t poll_sensor(void) {
  newData = 3;
  return newData;
}

static uint8_t receive_sensor_data(float *data, uint8_t size) {
  if (size >= 1) data[0] = rand() % 50;
  if (size >= 2) data[1] = rand() % 50;
  if (size >= 3) data[2] = rand() % 50;
  newData = 0;
  return 3;
}

static void init_sensor(uint8_t id, Evt_t *event, uint16_t poll_interval) {
  //// time_t t;
  sensor.info.id = id;
  sensor.info.event = event;
  sensor.info.poll_interval = poll_interval;
  //// srand((unsigned) time(&t));
}

SensorContext *setup_gyro_sensor(
  uint8_t id,
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, id, pollInterval, displayTaskID);
  return &sensorContext;
}

//  Not used.
static void next_channel(void) {}
