//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#ifndef BME280_H_
#define BME280_H_

#include "platform.h"
#ifdef SENSOR_DATA

#ifdef ARDUINO
#include <BME280I2C.h>
#endif  //  ARDUINO

BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

//  Set up the BME280 module for reading.
void bme280_setup(void);

#ifdef ARDUINO
//  Global instance of BME280 interface.
extern BME280I2C bme;
#endif  //  ARDUINO

END_EXTERN_C

#endif  //  SENSOR_DATA
#endif  //  BME280_H_
