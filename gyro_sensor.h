#ifndef GYRO_SENSOR_H_
#define GYRO_SENSOR_H_

#include "sensor.h"

#ifdef __cplusplus ////
extern "C" {
#endif ////

SensorContext *setup_gyro_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
);

#ifdef __cplusplus ////
}
#endif ////

#endif /* GYRO_SENSOR_H_ */
