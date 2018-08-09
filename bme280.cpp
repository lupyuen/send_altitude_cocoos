//  BME280 I2C interface for temperature, humidity and altitude sensors.
//  Requires the BME280 library to be installed through Arduino Library Manager:
//  https://github.com/finitespace/BME280
#include <Arduino.h>
#include <Wire.h>
#include <BME280I2C.h>
#include "bme280.h"

//  The global instance of the BME API.
BME280I2C bme;    // Default : forced mode, standby time = 1000 ms
                  // Oversampling = pressure ×1, temperature ×1, humidity ×1, filter off,

void bme280_setup(void) {
  //  Set up the BME280 module for reading.
  Wire.begin();
  while(!bme.begin()) {
    Serial.println(F("Could not find BME280 sensor!"));
    delay(1000);
  }
  switch(bme.chipModel()) {
     case BME280::ChipModel_BME280:
       Serial.println(F("Found BME280 sensor! Success."));
       break;
     case BME280::ChipModel_BMP280:
       Serial.println(F("Found BMP280 sensor! No Humidity available."));
       break;
     default:
       Serial.println(F("Found UNKNOWN sensor! Error!"));
  }
}
