//  Load the platform-dependent system include files, e.g. Arduino.h
//  Use platform.h instead of Arduino.h for portability.
#ifndef PLATFORM_H_
#define PLATFORM_H_

//  Configure the features according to the article.  Only one of these CONFIG_ARTICLEx should be defined.
//  #define CONFIG_ARTICLE1  //  Uncomment to support Article #1: "Juggling Arduino Sensors With cocoOS" https://medium.com/coinmonks/juggling-arduino-sensors-with-cocoos-403e14ec28be
//  #define CONFIG_ARTICLE2  //  Uncomment to support Article #2: "Juggling Sigfox Downlink And Arduino Sensors With cocoOS" https://medium.com/coinmonks/juggling-sigfox-downlink-and-arduino-sensors-with-cocoos-4594be59bf1b
//  #define CONFIG_ARTICLE3  //  Uncomment to support Article #3: "Juggling STM32 Blue Pill For Arduino Jugglers" https://medium.com/coinmonks/juggling-stm32-blue-pill-for-arduino-jugglers-edf6820dc808
//  #define CONFIG_ARTICLE4  //  Uncomment to support Article #4: "Watch STM32 Blue Pill Juggle Two SPI Sensors With DMA" https://medium.com/@ly.lee/watch-stm32-blue-pill-juggle-two-spi-sensors-with-dma-20cd1aa89869
#define CONFIG_ARTICLE5      //  Uncomment to support Article #5: "Connect STM32 Blue Pill to Sigfox"

//  Here are all the features that we may enable.  Warning: The features enabled in this section may be overridden according to the article configuration below.
#define USE_MICROPYTHON           //  Uncomment to include MicroPython.
//  #define USE_TEMP_EVENT_SENSOR //  Uncomment to use the new event-based temperature sensor.
#define USE_TEMP_POLLING_SENSOR   //  Uncomment to use the old polling-based temperature sensor.
#define USE_HUMIDITY_SENSOR       //  Uncomment to use the polling-based humidity sensor.
#define USE_ALTITUDE_SENSOR       //  Uncomment to use the polling-based altitude sensor.
#define USE_BME280_SPI            //  Uncomment to use SPI port to connect to BME280 instead of I2C. If SIMULATE_BME280 is defined, then USE_BME280_SPI has no effect because the BME280 simulator only works on I2C.
#define TRANSMIT_SENSOR_DATA      //  Uncomment to transmit sensor data to the IoT network (Sigfox).
//  #define USE_SIMULATOR         //  Uncomment to use the new Simulator for capturing, replaying and simulating SPI data for legacy Arduino code.  See Article #4.
//  #define SIMULATE_BME280       //  Uncomment to simulate a BME280 sensor (connected via I2C) instead of connecting to a real BME280 (I2C or SPI).  See Articles #3, #5.
//  #define SIMULATE_WISOL        //  Uncomment to simulate a Wisol Sigfox module connected to UART.  See Articles #3, #5.
//  #define SIMULATED_DATA        //  Uncomment to use hardcoded data. (May not work)
#define SENSOR_DATA               //  Uncomment to use data from real or simulated sensors instead of hardcoded data.

//  Uncomment to disable debug log for the following modules.
#define DISABLE_I2C_LOG
#define DISABLE_SENSOR_LOG
#define DISABLE_SIMULATOR_LOG
#define DISABLE_SPI_LOG
#define DISABLE_UART_LOG

#if defined(CONFIG_ARTICLE1) || defined(CONFIG_ARTICLE2) || defined(CONFIG_ARTICLE3)
//  Configuration for Article #1: "Juggling Arduino Sensors With cocoOS" https://medium.com/coinmonks/juggling-arduino-sensors-with-cocoos-403e14ec28be
//  and Article #2: "Juggling Sigfox Downlink And Arduino Sensors With cocoOS" https://medium.com/coinmonks/juggling-sigfox-downlink-and-arduino-sensors-with-cocoos-4594be59bf1b
//  and Article #3: "Juggling STM32 Blue Pill For Arduino Jugglers" https://medium.com/coinmonks/juggling-stm32-blue-pill-for-arduino-jugglers-edf6820dc808
//  Assume already defined above: TRANSMIT_SENSOR_DATA
#define USE_HUMIDITY_SENSOR  //  Use the polling-based humidity sensor.
#define USE_ALTITUDE_SENSOR  //  Use the polling-based altitude sensor.
#define SIMULATE_BME280      //  Simulate a BME280 sensor (connected via I2C) instead of connecting to a real BME280 (I2C or SPI).
#define SIMULATE_WISOL       //  Simulate a Wisol Sigfox module connected to UART.
#undef USE_TEMP_EVENT_SENSOR //  Disable the new event-based temperature sensor.
#undef USE_BME280_SPI        //  Use I2C instead of SPI port for BME280.

#elif defined(CONFIG_ARTICLE4)
//  Configuration for Article #4: "Watch STM32 Blue Pill Juggle Two SPI Sensors With DMA" https://medium.com/@ly.lee/watch-stm32-blue-pill-juggle-two-spi-sensors-with-dma-20cd1aa89869
//  Assume already defined above: USE_TEMP_POLLING_SENSOR, USE_BME280_SPI            
#undef USE_HUMIDITY_SENSOR        //  Don't use any real or simulated humidity sensor.
#undef USE_ALTITUDE_SENSOR        //  Don't use any real or simulated altitude sensor.
#undef TRANSMIT_SENSOR_DATA       //  Don't transmit any real or simulated sensor data to the IoT network (Sigfox).

#else
//  Else if the article configuration is not the above, assume we are configuring for the latest article.
//  Configuration for Article #5: "Connect STM32 Blue Pill to Sigfox"
//  We will connect to a real BME280 at port SPI1 or SPI2, and to a real Wisol Sigfox module at port UART2.
//  We will fetch real BME280 data for temperature, humidity, altitude via polling.  And send to Sigfox via the Wisol module.
//  Assume already defined above: USE_TEMP_POLLING_SENSOR, USE_HUMIDITY_SENSOR, USE_ALTITUDE_SENSOR, USE_BME280_SPI, TRANSMIT_SENSOR_DATA
//  Assume already undefined above: SIMULATE_BME280, SIMULATE_WISOL
//  Developer may define SIMULATE_BME280 or SIMULATE_WISOL to simulate BME280 or Wisol Sigfox module.

#endif  //  CONFIG_ARTICLE1, CONFIG_ARTICLE2, CONFIG_ARTICLE3, CONFIG_ARTICLE4

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
