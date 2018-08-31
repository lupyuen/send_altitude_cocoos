//  Instance of Sensor that reads the humidity sensor in BME280 I2C.
//  Caller must use a semaphore to prevent concurrent access to BME280 
//  module, which runs on a single I2C Bus.
#ifndef HUMID_SENSOR_H_
#define HUMID_SENSOR_H_

#include "platform.h"
#include "sensor.h"
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

SensorContext *setup_humid_sensor(  //  Set up the sensor parameters.
  uint16_t pollInterval,  //  How often the sensor should be polled, in milliseconds.
  uint8_t taskID   //  Task ID for the Network or Display Task.  Used for sending sensor data for aggregation/transmission or display.
);

END_EXTERN_C  //  End of extern C scope.
#endif  //  HUMID_SENSOR_H_
