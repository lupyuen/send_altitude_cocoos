//  Functions specific to the STM32 platform.  Called by main.cpp.
#ifndef BLUEPILL_H_
#define BLUEPILL_H_
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

void enable_debug(void);  //  Enable ARM Semihosting for displaying debug messages.
void disable_debug(void); //  Disable ARM Semihosting for displaying debug messages.
void platform_setup(void);  //  Initialise the STM32 platform.
void platform_start_timer(void (*tickFunc0)(void));  //  Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.

void led_setup(void);
void led_on(void);
void led_off(void);
void led_toggle(void);
void led_wait(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  BLUEPILL_H_
