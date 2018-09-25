//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#ifndef BME280_H_
#define BME280_H_

#include "platform.h"
#ifdef SENSOR_DATA

#ifdef STM32                     //  If we are running on STM32 Blue Pill...
#define BME280_SENSOR_PORT SPI1  //  Polling-based BME280 Sensor is connected at this SPI Port: SPI1 or SPI2.
#include <spiint.h>

#else                          //  In we are running on Arduino...
#define BME280_SENSOR_PORT 10  //  BME280 Sensor is connected at this pin for SPI Chip Select
#define spi_setup(id) NULL
#endif  //  STM32

#if defined(USE_BME280_SPI) && !defined(SIMULATE_BME280)  //  If we are using real SPI version of BME280...
#include <BME280Spi.h> //  Include the BME280 SPI declarations.
#else                  //  Else we are using I2C or simulated version of BME280...
#include <BME280I2C.h> //  Include the BME280 I2C declarations.
#endif  //  USE_BME280_SPI && !SIMULATE_BME280

BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

//  Set up the BME280 module for reading. Connect via that I/O port (I2C or SPI).
void bme280_setup(uint32_t port);

#if defined(USE_BME280_SPI) && !defined(SIMULATE_BME280)  //  If we are using real SPI version of BME280...
extern BME280Spi bme;  //  Declare global instance of BME280 SPI interface.
#else                  //  Else we are using I2C or simulated version of BME280...
extern BME280I2C bme;  //  Declare global instance of BME280 I2C interface.
#endif  //  USE_BME280_SPI && !SIMULATE_BME280

END_EXTERN_C
#endif  //  SENSOR_DATA
#endif  //  BME280_H_
