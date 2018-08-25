//  Load the platform-dependent system include files, e.g. Arduino.h
//  Also define the Serial buffer size.  Use platform.h instead of Arduino.h
//  for portability.
#ifndef PLATFORM_H_
#define PLATFORM_H_

//  Uncomment to use real sensor data instead of simulated data.
////#ifndef SENSOR_DATA
#define SENSOR_DATA
////#endif  //  !SENSOR_DATA
//  Uncomment to use simulated sensor data instead of real data.
//  #define SIMULATED_DATA

#define MAX_SENSOR_COUNT 3  //  Max number of sensors supported.
#define MAX_UART_SEND_MSG_SIZE 35  //  Max message length, e.g. 33 chars for AT$SF=0102030405060708090a0b0c,1\r
#define MAX_UART_RESPONSE_MSG_SIZE 36  //  Max response length, e.g. 36 chars for ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT\r

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

#ifdef ARDUINO  //  For Arduino only
#include <Arduino.h>
#endif  //  ARDUINO

#ifdef STM32  //  For STM32 only
#include <bluepill.h>
#include <stdint.h>  //  For uint32_t
#define __FlashStringHelper char
#define F(x) x  //  No need for flash memory helper on STM32.
#endif  //  STM32

#endif  //  PLATFORM_H_
