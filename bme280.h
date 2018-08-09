//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#ifndef BME280_H_
#define BME280_H_

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  Set up the BME280 module for reading.
void bme280_setup(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  BME280_H_
