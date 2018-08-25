//  I2C Interface for STM32. Compatible with Arduino's Wire I2C interface.
#define SIMULATE_BME280  //  Simulate a BME280 sensor connected to I2C Bus.
#include <logger.h>
#include "i2cint.h"

#ifdef SIMULATE_BME280  //  Simulate a BME280 sensor connected to I2C Bus.
//  BME280 Registers from https://github.com/finitespace/BME280/blob/master/src/BME280.h
static const uint8_t CTRL_HUM_ADDR   = 0xF2;
static const uint8_t CTRL_MEAS_ADDR  = 0xF4;
static const uint8_t CONFIG_ADDR     = 0xF5;
static const uint8_t PRESS_ADDR      = 0xF7;
static const uint8_t TEMP_ADDR       = 0xFA;
static const uint8_t HUM_ADDR        = 0xFD;
static const uint8_t TEMP_DIG_ADDR   = 0x88;
static const uint8_t PRESS_DIG_ADDR  = 0x8E;
static const uint8_t HUM_DIG_ADDR1   = 0xA1;
static const uint8_t HUM_DIG_ADDR2   = 0xE1;
static const uint8_t ID_ADDR         = 0xD0;

static const uint8_t TEMP_DIG_LENGTH         = 6;
static const uint8_t PRESS_DIG_LENGTH        = 18;
static const uint8_t HUM_DIG_ADDR1_LENGTH    = 1;
static const uint8_t HUM_DIG_ADDR2_LENGTH    = 7;
static const uint8_t DIG_LENGTH              = 32;
static const uint8_t SENSOR_DATA_LENGTH      = 8;

enum ChipModel {
    ChipModel_UNKNOWN = 0,
    ChipModel_BMP280 = 0x58,
    ChipModel_BME280 = 0x60
};

static const uint8_t ID_DATA[] = { ChipModel_BME280 };
static const uint8_t TEMP_DIG_DATA[] = { 54, 111, 194, 104, 50, 0 };
static const uint8_t HUM_DIG_DATA1[] = { 75 };
static const uint8_t HUM_DIG_DATA2[] = { 116, 1, 0, 18, 7, 0, 30 };
static const uint8_t PRESS_DATA[] = { 83, 212, 0, 133, 213, 0, 120, 141 };
static const uint8_t PRESS_DIG_DATA[] = { 14, 142, 139, 214, 208, 11, 156, 29, 1, 0, 249, 255, 172, 38, 10, 216, 189, 16 };

static uint8_t dataRegister = 0;  //  Register to be read.
static const uint8_t *data = NULL;  //  Simulated data to be returned for the register.
static uint8_t dataLength = 0;  //  Number of simulated bytes in data to be returned.
static uint8_t dataIndex = 0;  //  Current index to the simulated data to be returned.

size_t I2CInterface::write(uint8_t dataRegister0) {  //  Used by BME280I2C.cpp
    //  Simulate the handling of a request to read a register at "dataRegister0".
    //  debug_print("i2c_write reg: "); debug_println(dataRegister0);
    dataRegister = dataRegister0;
    return 1;
}

uint8_t I2CInterface::requestFrom(uint8_t addr, uint8_t length) {  //  Used by BME280I2C.cpp
    //  Simulate the handling of a request to read "length" number of bytes from the register at "dataRegister".
    //  debug_print("i2c_request reg: "); debug_println(dataRegister);
    dataIndex = 0;
    switch (dataRegister) {
        case ID_ADDR: data = ID_DATA; dataLength = (uint8_t) sizeof(ID_DATA); break;
        case TEMP_DIG_ADDR: data = TEMP_DIG_DATA; dataLength = (uint8_t) sizeof(TEMP_DIG_DATA); break;
        case HUM_DIG_ADDR1: data = HUM_DIG_DATA1; dataLength = (uint8_t) sizeof(HUM_DIG_DATA1); break;
        case HUM_DIG_ADDR2: data = HUM_DIG_DATA2; dataLength = (uint8_t) sizeof(HUM_DIG_DATA2); break;
        case PRESS_ADDR: data = PRESS_DATA; dataLength = (uint8_t) sizeof(PRESS_DATA); break;
        case PRESS_DIG_ADDR: data = PRESS_DIG_DATA; dataLength = (uint8_t) sizeof(PRESS_DIG_DATA); break;
        default: debug_print("Unknown i2c reg: "); debug_println(dataRegister); data = ID_DATA; dataLength = (uint8_t) sizeof(ID_DATA);
    }
    return dataLength;
}

int I2CInterface::available(void) {  //  Used by BME280I2C.cpp
    //  Return the number of simulated bytes to be read from the I2C address.
    return dataLength - dataIndex;
}

int I2CInterface::read(void) {  //  Used by BME280I2C.cpp
    //  Return the next simulated byte to be read from the I2C address.
    //  debug_println("i2c_read");
    if (dataIndex >= dataLength) { return -1; }
    return data[dataIndex++];
}
#endif  // SIMULATE_BME280

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
    return 1;
}
