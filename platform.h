//  Load the platform-dependent system include files, e.g. Arduino.h
//  Also define the Serial buffer size.  Use platform.h instead of Arduino.h
//  for portability.
#ifndef PLATFORM_H_
#define PLATFORM_H_

//  Uncomment to use data from real or simulated sensors instead of hardcoded data.
#define SENSOR_DATA
//  Uncomment to use hardcoded data.
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
#include <Arduino.h>        //  Every platform should define the common functions below plus millis()...
void enable_debug(void);    //  Enable display of debug messages.
void disable_debug(void);   //  Disable display of debug messages.
void platform_setup(void);  //  Initialise the Arduino platform.
void platform_start_timer(void (*tickFunc0)(void));  //  Start the Arduino Timer to generate interrupt ticks for cocoOS to perform task switching.

void led_setup(void);   //  Initialise the onboard LED.
void led_on(void);      //  Switch the onboard LED on.
void led_off(void);     //  Switch the onboard LED off.
void led_toggle(void);  //  Toggle the onboard LED on or off.
void led_wait(void);    //  Delay a while before updating the LED state.
#endif  //  ARDUINO

#ifdef STM32  //  For STM32 only
#include <bluepill.h>  //  For the platform, timer, millis(), debug and LED functions.
#include <stdint.h>  //  For uint32_t
#define __FlashStringHelper char
#define F(x) x  //  No need for flash memory helper on STM32.
#endif  //  STM32

#endif  //  PLATFORM_H_
