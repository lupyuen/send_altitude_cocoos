//  Define objects used for porting Arduino code to STM32.
#include <Wire.h>
#include <bluepill.h>
#include <spiint.h>

I2CInterface Wire;  //  Used by BME280 I2C library.
SPIInterface SPI;  //  Used by BME280 SPI Library.

//  Porting code moved to stm32/logger and stm32/bluepill
