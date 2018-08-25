//  Defines functions specific to the STM32 Blue Pill platform.
#include <libopencm3/cm3/cortex.h>
#include <libopencm3/stm32/rcc.h>
#include <logger.h>
#include "bluepill.h"

//  Debugging is off by default.  Developer must switch it on with enable_debug().
static bool debugEnabled = false;

void platform_setup(void) {
    //  Initialise the STM32 platform. At startup, the onboard LED will blink on-off-on-off-on and stays on.
	//  If LED blinks on-off-on-off and stays off, then debug mode is enabled and no debugger is connected.
	rcc_clock_setup_in_hse_8mhz_out_72mhz();
	led_setup();

	if (debugEnabled) {
		led_on(); led_wait();
		led_off(); led_wait();
		led_on(); led_wait();
		led_off(); led_wait();
		//  This line will call ARM Semihosting and may hang until debugger is connected.
  		debug_println("----platform_setup");
		led_on();
	}
}

void enable_debug(void) {
	//  Enable ARM Semihosting for displaying debug messages.
	debugEnabled = true;
	enable_log();
}

void disable_debug(void) {
	//  Disable ARM Semihosting for displaying debug messages.
	debugEnabled = false;
	disable_log();
}
