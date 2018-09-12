//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#include "platform.h"
#include "display.h"

#ifdef SENSOR_DATA
#include "bme280.h"
#include <Wire.h>

//  We will scan these I2C addresses for the BME280.
static const uint8_t i2cAddresses[] = {
  0x76,  //  Optional address for SparkFun BME280 Breakout Board.
  0x77,  //  Default address for SparkFun BME280 Breakout Board.
};

#ifdef USE_BME280_SPI  //  If we are using SPI version of BME280...

#ifdef STM32
#define DEVICE_PIN 1  //  STM32 SPI Port: 1=SPI1, 2=SPI2, 3=SPI3
#else
#define DEVICE_PIN 10  //  Arduino pin for SPI Chip Select
#endif  //  STM32

static BME280Spi::Settings settings(DEVICE_PIN);  //  BME280 SPI settings.
BME280Spi bme(settings); //  The global instance of the BME280 SPI API.

#else  //  Else we are using I2C version of BME280...
static BME280I2C::Settings settings;  //  BME280 I2C settings, including I2C address.
BME280I2C bme; //  The global instance of the BME280 I2C API.
#endif

// Default Settings: forced mode, standby time = 1000 ms
// Oversampling = pressure ×1, temperature ×1, humidity ×1, filter off,

void bme280_setup(SPI_Control *port) {
  //  Set up the BME280 module for reading.  Skip if already set up.
  static bool firstTime = true;
  if (!firstTime) return;  //  Already set up, quit.
  firstTime = false;
  Wire.begin();  //  Init the Wire library for BME280 library.

#ifdef USE_BME280_SPI  //  If we are using SPI version of BME280...
    //  Must configure the port before bme.begin() or the SPI code will hang at bme.begin().
    spi_setup(port, DEVICE_PIN);
    const int numAddresses = 1;  //  Check once only for SPI, which has no address.
#else  //  If we are using I2C version of BME280...
    //  Scan each I2C address for the BME280 module.
    const int numAddresses = sizeof(i2cAddresses) / sizeof(uint8_t);
#endif  //  !USE_BME280_SPI

  for (int i = 0; i < numAddresses; i++) {
#ifndef USE_BME280_SPI   //  If we are using I2C version of BME280...
    uint8_t addr = i2cAddresses[i];  //  Fetch the next I2C address to be scanned.
    settings.bme280Addr = addr;
    bme = BME280I2C(settings);
#endif  //  !USE_BME280_SPI
    //  Fetch the BME280 model number via I2C or SPI.  If not found at this I2C address, try next I2C address.
    if (!bme.begin()) { continue; }
    switch(bme.chipModel()) {
      case BME280::ChipModel_BME280:
        debug(F("BME280 OK"));
        return;
      case BME280::ChipModel_BMP280:
        debug(F("BMP280 without humidity"));
        return;
      default:
        debug(F("***** BME280 Error"));
        continue;  //  Try next address
    }
  }
  debug(F("BME280 not found"));
  delay(1000);  //  TODO: This loops forever on STM32.
}

#endif  //  SENSOR_DATA
