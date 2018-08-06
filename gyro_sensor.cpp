#include "sensor.h"
#include "gyro_sensor.h"
#include <string.h>
#include <cocoos.h>
#include <stdio.h>
#include <stdlib.h>

static uint8_t poll(void);
static uint8_t data(uint8_t *buf, uint8_t size);
static void init(uint8_t id, Evt_t *event, uint16_t period_ms);
static void next_channel(void);
static uint8_t x = 0;
static uint8_t y = 0;
static uint8_t z = 0;

static Sensor_t gyro_sensor = {
    .info.name = "Gyro Sensor",
    .info.event = 0,
    .info.id = 0,
    .info.period_ms = 0,
    .info.poll = &poll,
    .info.data = &data,
    .control.init = &init,
    .control.next_channel = &next_channel,
    .control.prev_channel = &next_channel
};

static uint8_t channel = 0;
static uint8_t newData = 0;


static uint8_t poll(void) {
  return newData;
}

static uint8_t data(uint8_t *buf, uint8_t size) {

  *buf++ = rand() % 50;
  *buf++ = rand() % 50;
  *buf++ = rand() % 50;

  newData = 0;
  return 3;
}

static void init(uint8_t id, Evt_t *event, uint16_t period_ms) {
  time_t t;
  gyro_sensor.info.id = id;
  gyro_sensor.info.event = event;
  gyro_sensor.info.period_ms = period_ms;
  srand((unsigned) time(&t));
}

static void next_channel(void) {
  channel = channel == 0 ? 1 : 0;

  newData = 1;
}

Sensor_t *gyroSensor_get(void) {
  return &gyro_sensor;
}

void gyroSensor_service(void) {
  static uint8_t cnt = 0;
  if (++cnt == 5) {
    newData = 1;
    cnt = 0;
  }

}
