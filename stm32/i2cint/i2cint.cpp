//  I2C Interface for STM32. Compatible with Arduino's Wire I2C interface.
#include "i2cint.h"

I2CInterface::I2CInterface() {
    //  TODO
}
void I2CInterface::begin() {  //  Used by bme280.cpp
    //  TODO
}
void I2CInterface::beginTransmission(uint8_t) {  //  Used by BME280I2C.cpp
    //  TODO
}
uint8_t I2CInterface::endTransmission(void) {  //  Used by BME280I2C.cpp
    //  TODO
    return -1;
}
uint8_t I2CInterface::requestFrom(uint8_t addr, uint8_t length) {  //  Used by BME280I2C.cpp
    //  TODO
    return -1;
}
size_t I2CInterface::write(uint8_t) {  //  Used by BME280I2C.cpp
    //  TODO
    return 0;
}
int I2CInterface::available(void) {  //  Used by BME280I2C.cpp
    //  TODO
    return -1;
}
int I2CInterface::read(void) {  //  Used by BME280I2C.cpp
    //  TODO
    return -1;
}
