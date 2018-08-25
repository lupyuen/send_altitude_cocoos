//  Functions specific to the STM32 Blue Pill platform.  Called by main.cpp.
#ifndef BLUEPILL_H_
#define BLUEPILL_H_
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

void enable_debug(void);    //  Enable ARM Semihosting for displaying debug messages.
void disable_debug(void);   //  Disable ARM Semihosting for displaying debug messages.
void platform_setup(void);  //  Initialise the STM32 Blue Pill platform.
void platform_start_timer(void (*tickFunc0)(void));  //  Start the STM32 Blue Pill Timer to generate interrupt ticks for cocoOS to perform task switching.

void led_setup(void);   //  Initialise the onboard LED.
void led_on(void);      //  Switch the onboard LED on.
void led_off(void);     //  Switch the onboard LED off.
void led_toggle(void);  //  Toggle the onboard LED on or off.
void led_wait(void);    //  Delay a while before updating the LED state.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  BLUEPILL_H_
