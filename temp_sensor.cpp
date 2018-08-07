#include "sensor.h"
#include "temp_sensor.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>

static uint8_t poll_sensor(void);
static uint8_t receive_sensor_data(float *data, uint8_t size);
static void init_sensor(uint8_t id, Evt_t *event, uint16_t period_ms);
static void next_channel(void);
static void prev_channel(void);

static SensorInfo sensor_info = {
  "tmp",
  0,
  0,
  0,
  &poll_sensor,
  &receive_sensor_data
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

static uint8_t channel = 0;
static uint8_t newData = 0;

static const char *channels[] = {
  "New York\t25 degC",
  "London\t\t18 degC",
  "Berlin\t\t20 degC",
  "Moscow\t\t12 degC",
  "Beijing\t\t28 degC",
  "Hong Kong\t34 degC",
  "Sydney\t\t42 degC"
};

static void init_sensor(uint8_t id, Evt_t *event, uint16_t poll_interval) {
  sensor.info.id = id;
  sensor.info.event = event;
  sensor.info.poll_interval = poll_interval;
}

static uint8_t poll_sensor(void) {
  //  Poll for new data. Return size of new data if new data available, 0 otherwise.
  //// debug("temp.poll_sensor"); ////
  newData = 1;  //  New sensor data now available.
  return newData;  //  Data should always be available.
}

static uint8_t receive_sensor_data(float *data, uint8_t size) {
  //// debug("temp.receive_sensor_data"); ////
  if (size >= 1) data[0] = 12.3 + rand() % 10;
  newData = 0;
  return 1;
}

static void next_channel(void) {
  channel++;
  uint8_t nChannels = sizeof(channels)/sizeof(channels[0]);
  if (channel == nChannels) {
    channel = 0;
  }
  event_ISR_signal(*sensor.info.event);
}

static void prev_channel(void) {
  if (channel == 0) {
    channel = sizeof(channels)/sizeof(channels[0]) -1;
  }
  else {
    channel--;
  }
  event_ISR_signal(*sensor.info.event);
}

Sensor *get_temp_sensor(void) {
  return &sensor;
}
