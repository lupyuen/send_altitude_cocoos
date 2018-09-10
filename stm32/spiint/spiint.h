//  SPI Interface for STM32. Compatible with Arduino's Wire SPI interface.
//  We support only Blue Pill SPI Port 1:
//  SS1   = Pin PA4
//  SCK1  = Pin PA5
//  MISO1 = Pin PA6
//  MOSI1 = Pin PA7
#ifndef SPIINT_H_
#define SPIINT_H_
#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  This is the new SPI Interface.  New code should use this.
void spi_setup(void);  	//  Enable SPI1 peripheral and GPIOA clocks.  Should be called once only.
void spi_configure();
void spi_open(void);  //  Enable DMA interrupt for SPI1.
int spi_transceive(uint8_t *tx_buf, int tx_len, uint8_t *rx_buf, int rx_len);
void spi_close(void);  //  Disable DMA interrupt for SPI1.
void spi_test(void);  //  For testing only.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#ifdef __cplusplus  //  SPIInterface class for C++ only
//  This is the legacy SPI Interface for Arduino.  New code should NOT use this.
class SPIInterface {
  public:
    SPIInterface();
    void begin();  //  Used by bme280.cpp
    void beginTransmission(uint8_t);  //  Used by BME280I2C.cpp
    size_t write(uint8_t registerIDOrValue);  //  Used by BME280I2C.cpp
    size_t write(uint8_t registerID, uint8_t value);  //  New code should use this.
    uint8_t endTransmission(void);  //  Used by BME280I2C.cpp
    uint8_t requestFrom(uint8_t addr, uint8_t length);  //  Used by BME280I2C.cpp
    int available(void);  //  Used by BME280I2C.cpp
    int read(void);  //  Used by BME280I2C.cpp
};
#endif  //  __cplusplus
#endif  //  SPIINT_H_
