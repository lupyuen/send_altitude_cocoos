//  Load the platform-dependent system include files, e.g. Arduino.h
//  Use platform.h instead of Arduino.h for portability.
#ifndef PLATFORM_H_
#define PLATFORM_H_

#define USE_TEMP_EVENT_SENSOR    //  Uncomment to use the event-based temperature sensor.
#define USE_TEMP_POLLING_SENSOR  //  Uncomment to use the polling-based temperature sensor.
//  #define USE_HUMIDITY_SENSOR  //  Uncomment to use the polling-based humidity sensor.
//  #define USE_ALTITUDE_SENSOR  //  Uncomment to use the polling-based altitude sensor.
//  #define TRANSMIT_SENSOR_DATA //  Uncomment to transmit sensor data to the IoT network (Sigfox).
#define USE_BME280_SPI         //  Uncomment to use SPI port to connect to BME280 instead of I2C.
#define SENSOR_DATA            //  Uncomment to use data from real or simulated sensors instead of hardcoded data.
//  #define SIMULATED_DATA     //  Uncomment to use hardcoded data.

#define MAX_SENSOR_COUNT 3             //  Max number of sensors supported.
#define MAX_PORT_COUNT 4               //  Max number of I/O ports that will be used, e.g. SPI1, I2C1.
#define MAX_UART_SEND_MSG_SIZE 35      //  Max message length, e.g. 33 chars for AT$SF=0102030405060708090a0b0c,1\r
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

//  Every platform should define the common functions below plus millis()...
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.
void enable_debug(void);    //  Enable display of debug messages.
void disable_debug(void);   //  Disable display of debug messages.
void platform_setup(void);  //  Initialise the Arduino or STM32 platform.
void platform_start_timer(void (*tickFunc0)(void));  //  Start the Arduino or STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.

void led_setup(void);   //  Initialise the onboard LED.
void led_on(void);      //  Switch the onboard LED on.
void led_off(void);     //  Switch the onboard LED off.
void led_toggle(void);  //  Toggle the onboard LED on or off.
void led_wait(void);    //  Delay a while before updating the LED state.
END_EXTERN_C  //  End of extern C scope.

#ifdef ARDUINO        //  If we are running on Arduino...
#include <Arduino.h>  //  Use the Arduino definition of millis()

#else  //  If we are running on STM32 or other platforms...
#include <stdint.h>  //  For uint32_t
#define __FlashStringHelper char  //  No need for flash memory helpers on STM32.
#define F(x) x
BEGIN_EXTERN_C          //  Allows functions below to be called by C and C++ code.
uint32_t millis(void);  //  Number of elapsed millisecond ticks. Compatible with Arduino.
END_EXTERN_C            //  End of extern C scope.
#endif  //  ARDUINO

#ifdef STM32  //  If we are running on STM32...
#include <bluepill.h>  //  We include the STM32 declarations to ensure they are consistent with above platform declarations.
#endif  //  STM32

#endif  //  PLATFORM_H_
