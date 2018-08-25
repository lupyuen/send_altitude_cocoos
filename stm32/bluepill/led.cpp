//  Blink code from https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include "bluepill.h"

void led_setup(void) {
	//  Set up Blue Pill LED GPIO.
	//  Enable GPIOC clock.
	rcc_periph_clock_enable(RCC_GPIOC);
	//  Set GPIO13 (in GPIO port C) to 'output push-pull'.
	gpio_set_mode(GPIOC, GPIO_MODE_OUTPUT_2_MHZ, GPIO_CNF_OUTPUT_PUSHPULL, GPIO13);
}

void led_on(void) {
	//  Switch Blue Pill LED on.
	gpio_clear(GPIOC, GPIO13);
}

void led_off(void) {
	//  Switch Blue Pill LED off.
	gpio_set(GPIOC, GPIO13);
}

void led_toggle(void) {
	//  Toggle Blue Pill LED.
	gpio_toggle(GPIOC, GPIO13);
}

void led_wait(void) {
	for (int i = 0; i < 1500000; i++)	/* Wait a bit. */
		__asm__("nop");
}
