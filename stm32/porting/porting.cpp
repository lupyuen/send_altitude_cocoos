//  Defines functions specific to the STM32 platform.
#include <Wire.h>

I2CInterface Wire;  //  Used by BME280 library.

//  Functions specific to the platform, e.g. STM32.  Called by main.cpp.
extern "C" void platform_setup(void);  //  Initialise the STM32 platform.
extern "C" void platform_start_timer(void);  //  Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.
extern "C" int test_main(void);  //  WARNING: test_main() never returns.

void platform_setup(void) {
    //  TODO: Initialise the STM32 platform.

    //  TODO: Do some STM32 testing for now. Will be removed.
    test_main();  //  WARNING: test_main() never returns.
}

void platform_start_timer(void) {
    //  TODO: Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.
}

//////////////////////////////////////////////////////////////////////////
//  STM32 Blue Pill Testing. From https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC

#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>

static void gpio_setup(void) {

	/* Enable GPIOC clock. */
	rcc_periph_clock_enable(RCC_GPIOC);

	/* Set GPIO8 (in GPIO port C) to 'output push-pull'. */
	gpio_set_mode(GPIOC,GPIO_MODE_OUTPUT_2_MHZ,
		      GPIO_CNF_OUTPUT_PUSHPULL,GPIO13);
}

int test_main(void) {
	//  We blink the Blue Pill onboard LED in a special pattern to distinguish ourselves
	//  from other blink clones - 2 x on, then 1 x off.
	int i;

	gpio_setup();

	for (;;) {
		gpio_clear(GPIOC,GPIO13);	/* LED on */
		for (i = 0; i < 1500000; i++)	/* Wait a bit. */
			__asm__("nop");

		gpio_clear(GPIOC,GPIO13);	/* LED on */
		for (i = 0; i < 1500000; i++)	/* Wait a bit. */
			__asm__("nop");

		gpio_set(GPIOC,GPIO13);		/* LED off */
		for (i = 0; i < 500000; i++)	/* Wait a bit. */
			__asm__("nop");
	}

	return 0;
}
