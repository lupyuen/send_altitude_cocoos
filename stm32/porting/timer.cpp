//  Based on https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/blob/master/rtos/rtc/main.c
#include <string.h>
#include <libopencm3/cm3/cortex.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/rtc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/cm3/nvic.h>

static volatile unsigned 
	rtc_isr_count = 0u,		// Times rtc_isr() called
	rtc_overflow_count = 0u;	// Times overflow occurred

//  This is the tick function we will call every millisecond.  
//  Usually points to os_tick() in cocoOS.
static void (*tickFunc)(void) = NULL;

/*********************************************************************
 * RTC Interrupt Service Routine
 *********************************************************************/

void rtc_isr(void) {
	++rtc_isr_count;
#ifdef NOTUSED
	if ( rtc_check_flag(RTC_OW) ) {
		// Timer overflowed:
		++rtc_overflow_count;
		rtc_clear_flag(RTC_OW);
		if (tickFunc != NULL) { tickFunc(); }
	} 
#endif
	if ( rtc_check_flag(RTC_SEC) ) {
		// RTC tick interrupt:
		rtc_clear_flag(RTC_SEC);
		if (tickFunc != NULL) { tickFunc(); }
		return;
	}
}

/*********************************************************************
 * Initialize RTC for Interrupt processing
 *********************************************************************/

static void rtc_setup(void) {
	rcc_enable_rtc_clock();
	rtc_interrupt_disable(RTC_SEC);
	rtc_interrupt_disable(RTC_ALR);
	rtc_interrupt_disable(RTC_OW);

	// RCC_HSE, RCC_LSE, RCC_LSI
	//// TODO: rtc_awake_from_off(RCC_HSE); 
	rtc_awake_from_off(HSE); //// TODO: Older version of libopencm3?
	
	rtc_set_prescale_val(62500);  //  1 second tick.
	// rtc_set_prescale_val(62);  //  Should be 62.5 for 1 millisecond tick.
	// rtc_set_counter_val(0xFFFFFFF0);  //  TODO.

	nvic_enable_irq(NVIC_RTC_IRQ);

	cm_disable_interrupts();
	rtc_clear_flag(RTC_SEC);
	rtc_clear_flag(RTC_ALR);
	rtc_clear_flag(RTC_OW);
	rtc_interrupt_enable(RTC_SEC);
	// rtc_interrupt_enable(RTC_OW);
	cm_enable_interrupts();
}

/*********************************************************************
 * Start Platform Timer
 *********************************************************************/

extern "C" void platform_start_timer(void (*tickFunc0)(void));  //  Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.

void platform_start_timer(void (*tickFunc0)(void)) {
    //  Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.
  	tickFunc = tickFunc0;
	rtc_setup();
}
