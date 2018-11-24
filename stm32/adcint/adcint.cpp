//  Based on https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/blob/master/rtos/adc/main.c
/* ADC1 - Read Temperature, Vref and a Single ADC input
 * Warren W. Gay	Thu Jul 13 22:25:31 2017
 *	GPIO	Function
 *	A0	Analog In
 *	A1	Analog In
 */
#include <string.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/adc.h>
#include <libopencm3/cm3/nvic.h>
#include "adcint.h"

//  TODO: Update this when running under FreeRTOS.
static void taskYIELD(void) {}

static uint16_t read_adc(uint8_t channel) {
 	//  Read ADC Channel
	adc_set_sample_time(ADC1,channel,ADC_SMPR_SMP_239DOT5CYC);
	adc_set_regular_sequence(ADC1,1,&channel);
	adc_start_conversion_direct(ADC1);
	while ( !adc_eoc(ADC1) )
		taskYIELD();
	return adc_read_regular(ADC1);
}

static int degrees_C100(void) {
 	//  Return temperature in C * 100
	static const int v25 = 143;
	int vtemp = (int)read_adc(ADC_CHANNEL_TEMP) * 3300 / 4095;
	return (v25 - vtemp) / 45 + 2500; // temp = (1.43 - Vtemp) / 4.5 + 25.00
}

static void demo_task(void *arg __attribute((unused))) {
	// int vref = read_adc(ADC_CHANNEL_VREF) * 330 / 4095;
	int adc0 = read_adc(0) * 330 / 4095;
	int adc1 = read_adc(1) * 330 / 4095;
	int temp = degrees_C100();
}

void adc_setup(void) {
	//  Moved to platform_setup() in bluepill.cpp:
	//  rcc_clock_setup_in_hse_8mhz_out_72mhz();
	rcc_periph_clock_enable(RCC_GPIOA);		// Enable GPIOA for ADC
	gpio_set_mode(GPIOA,
		GPIO_MODE_INPUT,
		GPIO_CNF_INPUT_ANALOG,
		GPIO0|GPIO1);				// PA0 & PA1
	// Initialize ADC:
	rcc_peripheral_enable_clock(&RCC_APB2ENR,RCC_APB2ENR_ADC1EN);
	adc_power_off(ADC1);
	rcc_peripheral_reset(&RCC_APB2RSTR,RCC_APB2RSTR_ADC1RST);
	rcc_peripheral_clear_reset(&RCC_APB2RSTR,RCC_APB2RSTR_ADC1RST);
	rcc_set_adcpre(RCC_CFGR_ADCPRE_PCLK2_DIV6);	// Set. 12MHz, Max. 14MHz
	adc_set_dual_mode(ADC_CR1_DUALMOD_IND);		// Independent mode
	adc_disable_scan_mode(ADC1);
	adc_set_right_aligned(ADC1);
	adc_set_single_conversion_mode(ADC1);
	adc_set_sample_time(ADC1,ADC_CHANNEL_TEMP,ADC_SMPR_SMP_239DOT5CYC);
	// adc_set_sample_time(ADC1,ADC_CHANNEL_VREF,ADC_SMPR_SMP_239DOT5CYC);
	adc_enable_temperature_sensor();
	adc_power_on(ADC1);
	adc_reset_calibration(ADC1);
	adc_calibrate_async(ADC1);
	while ( adc_is_calibrating(ADC1) );
	return 0;
}
