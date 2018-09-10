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

#define USE_16BIT_SPI_TRANSFERS 0  //  Uncomment for 8-bit SPI transfer.
//  #define USE_16BIT_SPI_TRANSFERS 1  //  Uncomment for 16-bit SPI transfer.

#ifndef LSBFIRST
#define LSBFIRST 0
#endif
#ifndef MSBFIRST
#define MSBFIRST 1
#endif

#define SPI_MODE0 0x00
#define SPI_MODE1 0x04
#define SPI_MODE2 0x08
#define SPI_MODE3 0x0C

#if USE_16BIT_SPI_TRANSFERS
#define SPI_DATA_TYPE uint16_t
#else
#define SPI_DATA_TYPE uint8_t
#endif

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  This is the new SPI Interface.  New code should use this.
void spi_setup(void);  	//  Enable SPI1 peripheral and GPIOA clocks.  Should be called once only.
void spi_configure(uint32_t clock, uint8_t bitOrder, uint8_t dataMode);
void spi_open(void);  //  Enable DMA interrupt for SPI1.
//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.
int spi_transceive(volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len);
int spi_transceive_wait(volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len);
void spi_wait(void);  //  Wait until transceive complete.
void spi_close(void);  //  Disable DMA interrupt for SPI1.
void spi_test(void);  //  For testing only.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#ifdef __cplusplus  //  SPIInterface class for C++ only
//  This is the legacy SPI Interface for Arduino.  New code should NOT use this.
class SPIInterfaceSettings {
public:
  SPIInterfaceSettings(uint32_t clock, uint8_t bitOrder, uint8_t dataMode);  //  Used by BME280Spi.cpp
  // SPIInterfaceSettings();
};

class SPIInterface {
public:
    // static void begin();
    // static void usingInterrupt(uint8_t interruptNumber);
    // static void notUsingInterrupt(uint8_t interruptNumber);
    static void beginTransaction(SPIInterfaceSettings settings);  //  Used by BME280Spi.cpp
    static uint8_t transfer(uint8_t data);  //  Used by BME280Spi.cpp
    // static uint16_t transfer16(uint16_t data);
    // static void transfer(void *buf, size_t count);
    static void endTransaction(void);  //  Used by BME280Spi.cpp
    // static void end();
    // static void setBitOrder(uint8_t bitOrder);
    // static void setDataMode(uint8_t dataMode);
    // static void setClockDivider(uint8_t clockDiv);
    // static void attachInterrupt();
    // static void detachInterrupt();
};
#endif  //  __cplusplus
#endif  //  SPIINT_H_
