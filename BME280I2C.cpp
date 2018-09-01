//Dummy implementation of BME280 driver

#include "BME280I2C.h"

const float pressures[] = {10.2, 20.2, 30.2};
const float temps[]     = {31.9, 35.9, 40.9};
const float hums[]      = {65.3, 75.3, 85.3};


BME280I2C::BME280I2C(): cnt(0) {}
float BME280I2C::pres(BME280::PresUnit u) {
  return pressures[cnt++ % 3];
}

float BME280I2C::temp(BME280::TempUnit u) {
  return temps[cnt++ % 3];
}

float BME280I2C::hum() {
  return hums[cnt++ % 3];
}
