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
  "tmp",  //  Name of sensor.
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

static uint8_t newData = 0;  //  Set to non-zero if there is new sensor data to be received.

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
}

static void prev_channel(void) {
}

Sensor *get_temp_sensor(void) {
  return &sensor;
}
