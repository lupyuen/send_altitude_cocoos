//  Define objects used for porting Arduino code to STM32.
#include <Wire.h>
#include <bluepill.h>
#include <spiint.h>
#include "wiring.h"

I2CInterface Wire;  //  Used by BME280 I2C library.
SPIInterface SPI;  //  Used by BME280 SPI Library.

void pinMode(uint8_t pin, uint8_t mode) {  //  Used by BME280Spi.h
    //  Intercept and remember the SPI port used by BME280 SPI Library.
    SPI.pinMode(pin, mode);
}

void digitalWrite(uint8_t pin, uint8_t val) {  //  Used by BME280Spi.h
    //  Intercept and remember the SPI port used by BME280 SPI Library.
    SPI.digitalWrite(pin, val);
}

//  Porting code moved to stm32/logger and stm32/bluepill
