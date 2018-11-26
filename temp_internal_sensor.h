//  Instance of Sensor that reads the STM32 Blue Pill internal temperature sensor via port ADC1.
#ifndef TEMP_INTERNAL_SENSOR_H_
#define TEMP_INTERNAL_SENSOR_H_

#include "platform.h"
#include "sensor.h"
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

SensorContext *setup_temperature_internal_sensor(  //  Set up the sensor parameters.
  uint16_t pollInterval,  //  How often the sensor should be polled, in milliseconds.
  uint8_t taskID   //  Task ID for the Network or Display Task.  Used for sending sensor data for aggregation/transmission or display.
);

END_EXTERN_C  //  End of extern C scope.
#endif  //  TEMP_INTERNAL_SENSOR_H_
