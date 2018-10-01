//  Stubs to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
//  Used by BME280 library.
#ifndef ARDUINO_H_
#define ARDUINO_H_

#include "../../platform.h"  //  For USE_MICROPYTHON
#include <stdint.h>  //  For uint8_t
#include <math.h>  //  For NAN used by BME280.cpp
#include "wiring.h"  //  Used by BME280 library
#include <spiint.h>  //  TODO: Move to SPI.h

//  TODO: Fix delay.  Used by bme280.cpp.
#define delay(x) // puts("**** Delay not implemented")

#ifdef USE_MICROPYTHON
//  BME280/EnvironmentCalculations.cpp code calls log() and pow() which are 0.9 KB and 2.7 KB.  We disable them for now.
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.
float disabled_log(float x);
float disabled_pow(float x, float y);
END_EXTERN_C  //  End of extern C scope.

#define log(x) disabled_log(x)
#define pow(x, y) disabled_pow(x, y)
#endif  //  USE_MICROPYTHON
#endif  //  ARDUINO_H_
