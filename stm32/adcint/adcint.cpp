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

static void demo_task(void *arg __attribute((unused))) {
	//  How to use ADC.
	int vref = adc_read_scaled_vref();
	int adc0 = adc_read(0) * 330 / 4095;
	int adc1 = adc_read(1) * 330 / 4095;
	int temp = adc_read_scaled_temperature();
}

uint16_t adc_read(uint8_t channel) {
	//  Read ADC1. Channel=0 for PA0, 1 for PA1, ADC_CHANNEL_TEMP for builtin temperature sensor, ADC_CHANNEL_VREFINT for voltage sensor.
	//  For PA0 and PA1, returned value should be multipled by 330 and divided by 4095, e.g. int adc0 = adc_read(0) * 330 / 4095;
	adc_set_sample_time(ADC1, channel, ADC_SMPR_SMP_239DOT5CYC);
	adc_set_regular_sequence(ADC1, 1, &channel);
	adc_start_conversion_direct(ADC1);
	while ( !adc_eoc(ADC1) )
		taskYIELD();
	return adc_read_regular(ADC1);
}

int adc_read_scaled_temperature(void) {
	//  Return Blue Pill temperature in degrees C scaled by 100 times, e.g. 36.9 C is returned as 3690.
	static const int v25 = 143;
	int vtemp = (int) adc_read(ADC_CHANNEL_TEMP) * 3300 / 4095;
	return (v25 - vtemp) / 45 + 2500; // temp = (1.43 - Vtemp) / 4.5 + 25.00
}

int adc_read_scaled_vref(void) {
	//  Return Blue Pill internal reference voltage in Volts scaled by 100 times, e.g. 1.23 V is returned as 123.
	int vref = adc_read(ADC_CHANNEL_VREFINT) * 330 / 4095;  //  Was ADC_CHANNEL_VREF
	return vref;
}

void adc_setup(void) {
	//  Configures ADC1 as ADC Controller, PA0 and PA1 as Analog In pins.  Enables built-in temperature sensor and voltage sensor.

	//  Moved to platform_setup() in bluepill.cpp:
	//  rcc_clock_setup_in_hse_8mhz_out_72mhz();
	rcc_periph_clock_enable(RCC_GPIOA);		// Enable GPIOA for ADC
	gpio_set_mode(GPIOA,
		GPIO_MODE_INPUT,
		GPIO_CNF_INPUT_ANALOG,
		GPIO0|GPIO1);				// PA0 & PA1
	// Initialize ADC.
	rcc_peripheral_enable_clock(&RCC_APB2ENR, RCC_APB2ENR_ADC1EN);
	adc_off(ADC1);  //  Was adc_power_off(ADC1)

	rcc_peripheral_reset(&RCC_APB2RSTR, RCC_APB2RSTR_ADC1RST);
	rcc_peripheral_clear_reset(&RCC_APB2RSTR, RCC_APB2RSTR_ADC1RST);
	rcc_set_adcpre(RCC_CFGR_ADCPRE_PCLK2_DIV6);	// Set. 12MHz, Max. 14MHz
	adc_set_dual_mode(ADC_CR1_DUALMOD_IND);		// Independent mode
	adc_disable_scan_mode(ADC1);
	adc_set_right_aligned(ADC1);
	adc_set_single_conversion_mode(ADC1);
	adc_set_sample_time(ADC1, ADC_CHANNEL_TEMP, ADC_SMPR_SMP_239DOT5CYC);
	adc_set_sample_time(ADC1, ADC_CHANNEL_VREFINT, ADC_SMPR_SMP_239DOT5CYC);
	adc_enable_temperature_sensor(ADC1);  //  TODO: Check para.

	adc_power_on(ADC1);
	adc_reset_calibration(ADC1);
	adc_calibration(ADC1);  //  Was adc_calibrate_async(ADC1)
	//  Was while (adc_is_calibrating(ADC1)) {}
}
