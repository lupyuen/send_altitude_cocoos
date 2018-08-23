#include <Wire.h>

I2CInterface Wire;  //  Used by BME280 library.

//  Functions specific to the platform, e.g. Arduino, STM32.  Called by main.cpp.
extern "C" void platform_setup(void);  //  Initialise the Arduino or STM32 platform.
extern "C" void platform_start_timer(void);  //  Start the Arduino or STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.

void platform_setup(void) {
    //  TODO
}

void platform_start_timer(void) {
    //  TODO
}
