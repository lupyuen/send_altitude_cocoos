#include "sensor.h"
#include "temp_sensor.h"
#include <string.h>
#include <cocoos.h>

static uint8_t poll_sensor(void);
static uint8_t receive_sensor_data(uint8_t *buf, uint8_t size);
static void init_sensor(uint8_t id, Evt_t *event, uint16_t period_ms);
static void next_channel(void);
static void prev_channel(void);

static SensorInfo sensor_info = {
  "BME280 Temperature Sensor",
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
static uint8_t newdata = 0;

const char *channels[] = {
                          "New York\t25 degC",
                          "London\t\t18 degC",
                          "Berlin\t\t20 degC",
                          "Moscow\t\t12 degC",
                          "Beijing\t\t28 degC",
                          "Hong Kong\t34 degC",
                          "Sydney\t\t42 degC"
                        };

static uint8_t poll_sensor(void) {
  return 1;
}

static uint8_t receive_sensor_data(uint8_t *buf, uint8_t size) {

  //// strcpy(buf, channels[channel]);
  strcpy((char *) buf, channels[channel]); ////

  return strlen(channels[channel]);
}

static void init_sensor(uint8_t id, Evt_t *event, uint16_t period_ms) {
  sensor.info.id = id;
  sensor.info.event = event;
  sensor.info.period_ms = period_ms;
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

void process_temp_sensor(void) {
  debug("process_temp_sensor"); ////
  static uint8_t cnt = 0;
  if (++cnt == 5) {
    newdata = 1;
    event_ISR_signal(*sensor.info.event);
    cnt = 0;
  }
}
