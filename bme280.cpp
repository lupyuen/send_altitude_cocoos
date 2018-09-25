//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Reuses the Arduino library for BME280, even on Blue Pill: https://github.com/finitespace/BME280
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

//  If we are simulating the BME280, ignore the SPI port because the simulator runs on I2C.

#if defined(USE_BME280_SPI) && !defined(SIMULATE_BME280)  //  If we are using real SPI version of BME280...
static BME280Spi::Settings settings(0);  //  BME280 SPI settings.
BME280Spi bme(settings); //  The global instance of the BME280 SPI API.

#else  //  Else we are using I2C or simulated version of BME280...
static BME280I2C::Settings settings;  //  BME280 I2C settings, including I2C address.
BME280I2C bme;                        //  The global instance of the BME280 I2C API.
#endif  //  USE_BME280_SPI && !SIMULATE_BME280

// Default Settings: forced mode, standby time = 1000 ms
// Oversampling = pressure ×1, temperature ×1, humidity ×1, filter off,

void bme280_setup(uint32_t port_id) {
  //  Set up the BME280 module for reading.  Skip if already set up.
  static bool firstTime = true;
  if (!firstTime) return;  //  Already set up, quit.
  firstTime = false;
  Wire.begin();  //  Init the Wire library for BME280 library.

#if defined(USE_BME280_SPI) && !defined(SIMULATE_BME280)  //  If we are using real SPI version of BME280...
    const int numAddresses = 1;  //  Check once only for SPI, which has no address.
#else  //  If we are using I2C version of BME280...
    const int numAddresses = sizeof(i2cAddresses) / sizeof(uint8_t);  //  Scan each I2C address for the BME280 module.
#endif  //  USE_BME280_SPI && !SIMULATE_BME280

  for (int i = 0; i < numAddresses; i++) {

#if defined(USE_BME280_SPI) && !defined(SIMULATE_BME280)  //  If we are using real SPI version of BME280...
    uint8_t pin = convert_port_to_pin(port_id);  //  Map the port to a pin.
    settings.spiCsPin = pin;                     //  Set the pin for the SPI port.
    bme = BME280Spi(settings);
#else                                //  If we are using I2C version of BME280...
    uint8_t addr = i2cAddresses[i];  //  Fetch the next I2C address to be scanned.
    settings.bme280Addr = addr;
    bme = BME280I2C(settings);
#endif  //  USE_BME280_SPI && !SIMULATE_BME280

    //  Fetch the BME280 model number via I2C or SPI.  If not found at this I2C address, try next I2C address.
    if (!bme.begin()) { continue; }
    switch(bme.chipModel()) {
      case BME280::ChipModel_BME280:
#ifdef SIMULATE_BME280  //  If we are using a simulated BME280...
        debug(F("bme >> simulated bme280"));  //  Show that we are simulated.
#else   //  Else we are using a real BME280...
        debug(F("bme >> real bme280 with arduino library"));
#endif  //  SIMULATE_BME280
        return;
      case BME280::ChipModel_BMP280:
        debug(F("bme >> bmp280 without humidity"));
        return;
      default:
        debug(F("***** bme280 Error"));
        continue;  //  Try next address
    }
  }
  debug(F("bme >> bme280 not found"));
  delay(1000);
}

#endif  //  SENSOR_DATA
