#ifndef TEMP_SENSOR_H_
#define TEMP_SENSOR_H_

#include "sensor.h"
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

SensorContext *setup_temp_sensor(
  uint16_t pollInterval,  //  How often the sensor should be polled, in milliseconds.
  uint8_t displayTaskID  //  Task ID for the Display Task.  Used for sending display messages.
);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#endif  //  TEMP_SENSOR_H_
