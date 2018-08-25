//  Based on https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/blob/master/rtos/rtc/main.c
#include <string.h>
#include <libopencm3/cm3/cortex.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/rtc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/cm3/nvic.h>
#include "bluepill.h"

//  This is the tick function we will call every millisecond.  
//  Usually points to os_tick() in cocoOS.
static void (*tickFunc)(void) = NULL;
static volatile uint32_t tickCount = 0;  //  Number of millisecond ticks elapsed.

uint32_t millis(void) {
	//  Return the number of millisecond ticks since startup.
	//  Compatible with Arduino's millis() function.
	return tickCount;
}

/*********************************************************************
 * RTC Interrupt Service Routine
 *********************************************************************/

void rtc_isr(void) {
	if (rtc_check_flag(RTC_SEC)) {
		//  We hit an RTC tick interrupt.
		rtc_clear_flag(RTC_SEC);
		tickCount++;
		//  Call the tick function os_tick() for cocoOS to perform multitasking.
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
	
	rtc_set_prescale_val(62);  //  1 millisecond tick: Should be 62.5
	// rtc_set_prescale_val(625);  //  0.01 second tick
	// rtc_set_prescale_val(6250);  //  0.1 second tick
	// rtc_set_prescale_val(62500);  //  1 second tick

	nvic_enable_irq(NVIC_RTC_IRQ);

	cm_disable_interrupts();
	rtc_clear_flag(RTC_SEC);
	rtc_clear_flag(RTC_ALR);
	rtc_clear_flag(RTC_OW);
	rtc_interrupt_enable(RTC_SEC);
	cm_enable_interrupts();
}

/*********************************************************************
 * Start Platform Timer
 *********************************************************************/

void platform_start_timer(void (*tickFunc0)(void)) {
    //  Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.
  	tickFunc = tickFunc0;
	rtc_setup();
}
