//  Functions specific to the STM32 Blue Pill platform.  Called by main.cpp.
#ifndef BLUEPILL_H_
#define BLUEPILL_H_
#include <stdint.h>  //  For uint32_t
#include <libopencm3/stm32/spi.h>    //  For SPI port definitions e.g. SPI1
#include <libopencm3/stm32/i2c.h>    //  For I2C port definitions e.g. I2C1
#include <libopencm3/stm32/usart.h>  //  For USART port definitions e.g. USART1
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

void enable_debug(void);    //  Enable ARM Semihosting for displaying debug messages.
void disable_debug(void);   //  Disable ARM Semihosting for displaying debug messages.
void platform_setup(void);  //  Initialise the STM32 Blue Pill platform.
void platform_start_timer(void (*tickFunc0)(void));  //  Start the STM32 Blue Pill Timer to generate interrupt ticks for cocoOS to perform task switching.
uint32_t millis(void);  //  Number of elapsed millisecond ticks. Compatible with Arduino.

void led_setup(void);   //  Initialise the onboard LED.
void led_on(void);      //  Switch the onboard LED on.
void led_off(void);     //  Switch the onboard LED off.
void led_toggle(void);  //  Toggle the onboard LED on or off.
void led_wait(void);    //  Delay a while before updating the LED state.

//  For Legacy Arduino Support only...
uint32_t convert_pin_to_port(uint8_t pin);      //  Map Arduino Pin to STM32 Port, e.g. 1 becomes SPI1
uint8_t convert_port_to_pin(uint32_t port_id);  //  Map STM32 port to Arduino Pin, e.g. SPI1 becomes 1

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  BLUEPILL_H_
