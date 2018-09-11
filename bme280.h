//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#ifndef BME280_H_
#define BME280_H_

#include "platform.h"
#ifdef SENSOR_DATA

#ifdef USE_BME280_SPI  //  If we are using SPI version of BME280...
#include <BME280Spi.h>
#else  //  Else we are using I2C version of BME280...
#include <BME280I2C.h>
#endif

BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

//  Set up the BME280 module for reading.
void bme280_setup(void);

#ifdef USE_BME280_SPI  //  If we are using SPI version of BME280...
extern BME280Spi bme;  //  Global instance of BME280 SPI interface.
#else  //  Else we are using I2C version of BME280...
extern BME280I2C bme;  //  Global instance of BME280 I2C interface.
#endif

END_EXTERN_C

#endif  //  SENSOR_DATA
#endif  //  BME280_H_
