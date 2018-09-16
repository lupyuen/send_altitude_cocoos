//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#ifndef BME280_H_
#define BME280_H_

#include "platform.h"
#ifdef SENSOR_DATA

#ifdef STM32  //  If we are running on STM32 Blue Pill...
#define SENSOR_PORT 2  //  Sensor is connected at this SPI Port (1=SPI1, 2=SPI2)
#include <spiint.h>

#else  //  In we are running on Arduino...
#define SENSOR_PORT 10  //  Sensor is connected at this pin for SPI Chip Select
#define spi_setup(id) NULL
#endif  //  STM32

#ifdef USE_BME280_SPI  //  If we are using SPI version of BME280...
#include <BME280Spi.h> //  Include the BME280 SPI declarations.
#else                  //  Else we are using I2C version of BME280...
#include <BME280I2C.h> //  Include the BME280 I2C declarations.
#endif

BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

//  Set up the BME280 module for reading. Connect via that I/O port (I2C or SPI).
void bme280_setup(uint8_t port);

#ifdef USE_BME280_SPI  //  If we are using SPI version of BME280...
extern BME280Spi bme;  //  Declare global instance of BME280 SPI interface.
#else                  //  Else we are using I2C version of BME280...
extern BME280I2C bme;  //  Declare global instance of BME280 I2C interface.
#endif

END_EXTERN_C
#endif  //  SENSOR_DATA
#endif  //  BME280_H_
