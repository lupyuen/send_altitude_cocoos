//  Load the platform-dependent system include files, e.g. Arduino.h
//  Also define the Serial buffer size.  Use platform.h instead of Arduino.h
//  for portability.
#ifndef PLATFORM_H_
#define PLATFORM_H_

#define SENSOR_DATA

#define N_SENSORS 1                     // Number of sensors used
#define SENSOR_DATA_SIZE 2              // Number of values from each sensor

#define TICKS_PER_S  1000

#define TRANSMIT_SIG 67
#define SENSOR_DATA_SIG 68

//  Wrap function and global variable declarations with BEGIN_EXTERN_C ... END_EXTERN_C so that
//  the functions and global variables may be used in both C and C++ code.
#ifdef __cplusplus
#define BEGIN_EXTERN_C extern "C" {
#define END_EXTERN_C }
#else
#define BEGIN_EXTERN_C
#define END_EXTERN_C
#endif  //  __cplusplus

#if defined(STM32F0) || defined(STM32F1) || defined(STM32F2) || defined(STM32F3) || defined(STM32F4) || defined(STM32L0) || defined(STM32L1)
#define STM32  //  For STM32 platforms
#endif



#ifdef STM32  //  For STM32 only

#define __FlashStringHelper char
#define F(x) x  //  No need for flash memory helper on STM32.
#define millis() (tickCount)  //  Return timestamp in milliseconds.
#include <stdint.h>  //  For uint32_t

//Print Serial;
#endif  //  STM32

BEGIN_EXTERN_C
extern volatile uint32_t tickCount;  //  Number of millisecond ticks.
END_EXTERN_C

#endif  //  PLATFORM_H_
