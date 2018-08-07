#include <Arduino.h> ////
#include <Time.h> ////
#include <TimeLib.h> ////
typedef unsigned long time_t; //// TODO: Fix this declaration

#include "sensor.h"
#include "gyro_sensor.h"
#include <string.h>
#include <cocoos.h>
#include <stdio.h>
#include <stdlib.h>

static uint8_t poll_sensor(void);
static uint8_t receive_sensor_data(float *data, uint8_t size);
static void init_sensor(uint8_t id, Evt_t *event, uint16_t period_ms);
static void next_channel(void);
static uint8_t x = 0;
static uint8_t y = 0;
static uint8_t z = 0;

static SensorInfo gyro_sensor_info = {
  "gyr",
  0,
  0,
  0,
  &poll_sensor,
  &receive_sensor_data
};

static SensorControl gyro_sensor_control = {
  &init_sensor,
  &next_channel,
  &next_channel
};

static Sensor gyro_sensor = {
  gyro_sensor_info,
  gyro_sensor_control
};

static uint8_t channel = 0;
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
  gyro_sensor.info.id = id;
  gyro_sensor.info.event = event;
  gyro_sensor.info.poll_interval = poll_interval;
  //// srand((unsigned) time(&t));
}

static void next_channel(void) {
  channel = channel == 0 ? 1 : 0;
  newData = 1;
}

Sensor *gyroSensor_get(void) {
  return &gyro_sensor;
}
