//  Define objects used for porting Arduino code to STM32.
#include "../../platform.h"  //  For USE_MICROPYTHON
#include <Wire.h>
#include <bluepill.h>
#include <spiint.h>
#include <logger.h>
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

#ifdef USE_MICROPYTHON
//  BME280/EnvironmentCalculations.cpp code calls log() and pow() which are 0.9 KB and 2.7 KB.  We disable them for now.

extern "C" float disabled_log(float x) {
    debug_println("*** ERROR: BME280 Arduino Library calls log() which is disabled");  debug_flush();
    return 1;
}

extern "C" float disabled_pow(float x, float y) {
    debug_println("*** ERROR: BME280 Arduino Library calls pow() which is disabled");  debug_flush();
    return 1;    
}
#endif  //  USE_MICROPYTHON

//  Porting code moved to stm32/logger and stm32/bluepill
