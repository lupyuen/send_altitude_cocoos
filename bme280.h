//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#ifndef BME280_H_
#define BME280_H_
#ifdef SENSOR_DATA
#include <BME280I2C.h>

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  Set up the BME280 module for reading.
void bme280_setup(void);

//  Global instance of BME280 interface.
extern BME280I2C bme;

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#endif  //  SENSOR_DATA
#endif  //  BME280_H_
