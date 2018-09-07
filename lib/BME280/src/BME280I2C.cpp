/*
BME280I2CI2C.cpp
This code records data from the BME280I2C sensor and provides an API.
This file is part of the Arduino BME280I2C library.
Copyright (C) 2016  Tyler Glenn

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Written: Dec 30 2015.
Last Updated: Jan 1 2016. - Happy New year!

This header must be included in any derived code or copies of the code.

Based on the data sheet provided by Bosch for the BME280I2C environmental sensor,
calibration code based on algorithms providedBosch, some unit conversations courtesy
of www.endmemo.com, altitude equation courtesy of NOAA, and dew point equation
courtesy of Brian McNoldy at http://andrew.rsmas.miami.edu.
 */

#include <Wire.h>

#include "BME280I2C.h"
#ifndef ARDUINO
#include <logger.h>
#endif  //  !ARDUINO


/****************************************************************/
BME280I2C::BME280I2C
(
  const Settings& settings
):BME280(settings),
  m_bme_280_addr(settings.bme280Addr)
{
}


/****************************************************************/
bool BME280I2C::WriteRegister
(
  uint8_t addr,
  uint8_t data
)
{
#ifndef ARDUINO
  //  debug_print("BME280I2C::WriteRegister addr="); debug_print((int) m_bme_280_addr); debug_print(", reg="); debug_print((int) addr); debug_print(", data="); debug_println((int) data); debug_flush(); ////
#endif  //  !ARDUINO
  Wire.beginTransmission(m_bme_280_addr);
  Wire.write(addr);
  Wire.write(data);
  Wire.endTransmission();
#ifndef ARDUINO
  //  debug_println("BME280I2C::WriteRegister done"); debug_flush(); ////
#endif  //  !ARDUINO

  return true; // TODO: Chech return values from wire calls.
}


/****************************************************************/
bool BME280I2C::ReadRegister
(
  uint8_t addr,
  uint8_t data[],
  uint8_t length
)
{
  uint8_t ord(0);

#ifndef ARDUINO
  //  debug_print("BME280I2C::ReadRegister addr="); debug_print((int) m_bme_280_addr); debug_print(", reg="); debug_print((int) addr); debug_print(", length="); debug_println((int) length); debug_flush(); ////
#endif  //  !ARDUINO
  Wire.beginTransmission(m_bme_280_addr);
  Wire.write(addr);
  Wire.endTransmission();

  Wire.requestFrom(m_bme_280_addr, length);

  while(Wire.available())
  {
    data[ord++] = Wire.read();
  }

#ifdef NOTUSED
#ifndef ARDUINO
  debug_println("BME280I2C::ReadRegister result: ");
    for (int i = 0; i < ord; i++) {
    debug_println((int) data[i]);
  }
  debug_flush();
#endif  //  !ARDUINO
#endif  //  NOTUSED

// #define TRACE_BME280  //  Trace the BME280 library.
#ifdef TRACE_BME280
#ifdef ARDUINO
#define debug_println(x) Serial.println(x)
#define debug_flush() Serial.flush()
#endif  //  ARDUINO
  debug_println("bme280i2c_read: addr=");
  debug_println((int) addr);
  debug_println("bme280i2c_read: ord=");
  debug_println((int) ord);
  debug_println("bme280i2c_read: data=");
  for (int i = 0; i < ord; i++) {
    debug_println((int) data[i]);
  }
  debug_flush();
#endif  //  TRACE_BME280
  return ord == length;
}
