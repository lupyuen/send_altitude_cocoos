//  ADC Interface for STM32
#ifndef ADCINT_H_
#define ADCINT_H_
#include <stdint.h>  //  For uint16_t

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  Configures ADC1 as ADC Controller, PA0 and PA1 as Analog In pins.  Enables built-in temperature sensor and voltage sensor.
void adc_setup(void);

//  Read the ADC Controller. Channel=0 for PA0, 1 for PA1, ADC_CHANNEL_TEMP for builtin temperature sensor, ADC_CHANNEL_VREFINT for voltage sensor.
//  For PA0 and PA1, returned value should be multipled by 3.30 and divided by 4095.0, e.g. float adc0 = adc_read(0) * 3.30 / 4095.0;
uint16_t adc_read(uint8_t channel);

//  Return Blue Pill internal temperature in degrees C scaled by 100 times, e.g. 36.9 C is returned as 3690.
int adc_read_scaled_temperature(void);

//  Return Blue Pill internal reference voltage in Volts scaled by 100 times, e.g. 1.23 V is returned as 123.
int adc_read_scaled_vref(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  ADCINT_H_
