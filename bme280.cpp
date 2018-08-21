//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#include "platform.h"
#include "display.h"

#ifdef SENSOR_DATA
#include "bme280.h"
#include <Wire.h>

//  The global instance of the BME API.
BME280I2C bme;    // Default : forced mode, standby time = 1000 ms
                  // Oversampling = pressure ×1, temperature ×1, humidity ×1, filter off,

void bme280_setup(void) {
  //  Set up the BME280 module for reading.  Skip if already set up.
  static bool firstTime = true;
  if (!firstTime) return;  //  Already set up, quit.
  firstTime = false;
  
  //Wire.begin();
  while(!bme.begin()) {
    debug(F("BME280 not found"));
    //delay(1000);
  }
  switch(bme.chipModel()) {
     case BME280::ChipModel_BME280:
       debug(F("BME280 OK"));
       break;
     case BME280::ChipModel_BMP280:
       debug(F("BME280 without humidity"));
       break;
     default:
       debug(F("***** BME280 Error"));
  }
}

#endif  //  SENSOR_DATA
