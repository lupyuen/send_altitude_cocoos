//  Stubs to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
//  Used by BME280 library.
#ifndef ARDUINO_H_
#define ARDUINO_H_

#include <stdint.h>  //  For uint8_t
#include <math.h>  //  For NAN used by BME280.cpp
#include "wiring.h"  //  Used by BME280 library
#include <spiint.h>  //  TODO: Move to SPI.h

//  TODO: Fix delay.  Used by bme280.cpp.
#define delay(x) // puts("**** Delay not implemented")

#endif  //  ARDUINO_H_
