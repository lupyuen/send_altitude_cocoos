#ifndef ALT_SENSOR_H_
#define ALT_SENSOR_H_

#include "sensor.h"

#ifdef __cplusplus ////
extern "C" {
#endif ////

SensorContext *setup_alt_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
);

#ifdef __cplusplus ////
}
#endif ////

#endif /* ALT_SENSOR_H_ */
