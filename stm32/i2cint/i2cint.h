//  I2C Interface for STM32. Compatible with Arduino's Wire I2C interface.
#ifndef I2CINT_H_
#define I2CINT_H_
#ifdef __cplusplus  //  I2CInterface class for C++ only

#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t

class I2CInterface {
  public:
    I2CInterface();
    void begin();  //  Used by bme280.cpp
    void beginTransmission(uint8_t);  //  Used by BME280I2C.cpp
    uint8_t endTransmission(void);  //  Used by BME280I2C.cpp
    uint8_t requestFrom(uint8_t addr, uint8_t length);  //  Used by BME280I2C.cpp
    int available(void);  //  Used by BME280I2C.cpp
    int read(void);  //  Used by BME280I2C.cpp
    size_t write(uint8_t);  //  Used by BME280I2C.cpp
};

#endif  //  __cplusplus
#endif  //  I2CINT_H_
