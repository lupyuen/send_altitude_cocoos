//  Stubs to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
//  SPI Interface used by BME280 library but implementation not needed.
#ifndef SPI_H_
#define SPI_H_

#include <stdint.h>  //  For uint8_t
#include "wiring.h"

//  From Arduino SPI.h

// SPI_HAS_TRANSACTION means SPI has beginTransaction(), endTransaction(),
// usingInterrupt(), and SPISetting(clock, bitOrder, dataMode)
#define SPI_HAS_TRANSACTION 1

// SPI_HAS_NOTUSINGINTERRUPT means that SPI has notUsingInterrupt() method
#define SPI_HAS_NOTUSINGINTERRUPT 1

// SPI_ATOMIC_VERSION means that SPI has atomicity fixes and what version.
// This way when there is a bug fix you can check this define to alert users
// of your code if it uses better version of this library.
// This also implies everything that SPI_HAS_TRANSACTION as documented above is
// available too.
#define SPI_ATOMIC_VERSION 1

// Uncomment this line to add detection of mismatched begin/end transactions.
// A mismatch occurs if other libraries fail to use SPI.endTransaction() for
// each SPI.beginTransaction().  Connect an LED to this pin.  The LED will turn
// on if any mismatch is ever detected.
//#define SPI_TRANSACTION_MISMATCH_LED 5

#ifndef LSBFIRST
#define LSBFIRST 0
#endif
#ifndef MSBFIRST
#define MSBFIRST 1
#endif

#define SPI_CLOCK_DIV4 0x00
#define SPI_CLOCK_DIV16 0x01
#define SPI_CLOCK_DIV64 0x02
#define SPI_CLOCK_DIV128 0x03
#define SPI_CLOCK_DIV2 0x04
#define SPI_CLOCK_DIV8 0x05
#define SPI_CLOCK_DIV32 0x06

#define SPI_MODE0 0x00
#define SPI_MODE1 0x04
#define SPI_MODE2 0x08
#define SPI_MODE3 0x0C

#define SPI_MODE_MASK 0x0C  // CPOL = bit 3, CPHA = bit 2 on SPCR
#define SPI_CLOCK_MASK 0x03  // SPR1 = bit 1, SPR0 = bit 0 on SPCR
#define SPI_2XCLOCK_MASK 0x01  // SPI2X = bit 0 on SPSR

// define SPI_AVR_EIMSK for AVR boards with external interrupt pins
#if defined(EIMSK)
  #define SPI_AVR_EIMSK  EIMSK
#elif defined(GICR)
  #define SPI_AVR_EIMSK  GICR
#elif defined(GIMSK)
  #define SPI_AVR_EIMSK  GIMSK
#endif

class SPISettings {
public:
  SPISettings(uint32_t clock, uint8_t bitOrder, uint8_t dataMode);  //  Used by BME280Spi.cpp
  // SPISettings();
};

class SPIClass {
public:
    // static void begin();
    // static void usingInterrupt(uint8_t interruptNumber);
    // static void notUsingInterrupt(uint8_t interruptNumber);
    static void beginTransaction(SPISettings settings);  //  Used by BME280Spi.cpp
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

extern SPIClass SPI;

#endif  //  SPI_H_
