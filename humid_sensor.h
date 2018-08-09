#ifndef HUMID_SENSOR_H_
#define HUMID_SENSOR_H_

#include "sensor.h"

#ifdef __cplusplus ////
extern "C" {
#endif ////

SensorContext *setup_humid_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
);

#ifdef __cplusplus ////
}
#endif ////

#endif /* HUMID_SENSOR_H_ */
