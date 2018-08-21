//  Declarations to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
//  Used by BME280 library.
#ifndef ARDUINO_H_
#define ARDUINO_H_

#include <stdint.h>  //  For uint8_t
#include <stdio.h>  //  TODO: Remove when puts() is fixed

//  TODO: Fix delay.  Used by bme280.cpp.
#define delay(x) puts("**** Delay not implemented")

#endif  //  ARDUINO_H_
