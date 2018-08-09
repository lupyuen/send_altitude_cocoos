//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#include <Arduino.h>
#include <Wire.h>
#include "bme280.h"

//  TODO: Print a message to the Arduino serial console. Defined in main.cpp.
extern "C" void debug(
  const char *s1,  //  Text to be printed.
  const char *s2  //  Another text to be printed.
  #ifdef __cplusplus
    = 0  //  Second parameter may be omitted.
  #endif
  );

//  The global instance of the BME API.
BME280I2C bme;    // Default : forced mode, standby time = 1000 ms
                  // Oversampling = pressure ×1, temperature ×1, humidity ×1, filter off,

void bme280_setup(void) {
  //  Set up the BME280 module for reading.  Skip if already set up.
  static bool firstTime = true;
  if (!firstTime) return;  //  Already set up, quit.
  firstTime = false;
  
  Wire.begin();
  while(!bme.begin()) {
    debug("BME280 not found");
    delay(1000);
  }
  switch(bme.chipModel()) {
     case BME280::ChipModel_BME280:
       debug("BME280 OK");
       break;
     case BME280::ChipModel_BMP280:
       debug("BME280 without humidity");
       break;
     default:
       debug("BME280 Error");
  }
}
