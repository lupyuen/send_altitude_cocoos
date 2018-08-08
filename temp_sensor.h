#ifndef TEMP_SENSOR_H_
#define TEMP_SENSOR_H_

#include "sensor.h"

#ifdef __cplusplus ////
extern "C" {
#endif ////

SensorContext *setup_temp_sensor(
  uint8_t id,
  uint16_t pollInterval,
  uint8_t displayTaskID
);

#ifdef __cplusplus ////
}
#endif ////

#endif /* TEMP_SENSOR_H_ */
