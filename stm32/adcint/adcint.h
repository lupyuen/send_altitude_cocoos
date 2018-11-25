//  ADC Interface for STM32
#ifndef ADCINT_H_
#define ADCINT_H_
#include <stdint.h>  //  For uint16_t

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  Configures ADC1 as ADC Input, PA0 and PA1 as Analog In pins.
void adc_setup(void);

//  Read ADC1. Channel=0 for PA0, 1 for PA1.
uint16_t adc_read(uint8_t channel);

//  Return Blue Pill temperature in degrees C scaled by 100 times, e.g. 36.9 C is returned as 3690.
int adc_read_scaled_temperature(void);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  ADCINT_H_
