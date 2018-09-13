//  SPI Interface for STM32. Compatible with Arduino's Wire SPI interface.
//  The SPI Interface supports the Simulator Module that captures, replays and simulates SPI commands for SPI sensors.
//  We do this so that we can capture the SPI send/receive commands of Arduino sensor drivers and replay
//  them efficiently on STM32, with multitasking.  And also for testing with simulated sensors.
//  We support only Blue Pill SPI Port 1:
//  SS1   = Pin PA4
//  SCK1  = Pin PA5
//  MISO1 = Pin PA6
//  MOSI1 = Pin PA7
#ifndef SPIINT_H_
#define SPIINT_H_
#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t
#include <cocoos.h>  //  For Evt_t

#define MAX_SPI_PORTS 3  //  Define 3 SPI ports: SPI1, SPI2, SPI3.

#define USE_16BIT_SPI_TRANSFERS 0  //  Uncomment for 8-bit SPI transfer.
//  #define USE_16BIT_SPI_TRANSFERS 1  //  Uncomment for 16-bit SPI transfer.

//  From Arduino.h.  Used by BME280ASpi.h.
#define HIGH 0x1
#define LOW  0x0

#define INPUT 0x0
#define OUTPUT 0x1

#define LSBFIRST 0
#define MSBFIRST 1

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

enum SPI_Fails {  //  Error codes.
	SPI_Ok = 0,
  SPI_Missing_Simulator,
	SPI_Invalid_Port,
	SPI_Invalid_Size,
	SPI_Mismatch,
	SPI_Timeout,
  SPI_End,  //  Insert new codes above.
};

/* This is for the counter state flag */
enum cnt_state {
	TX_UP_RX_HOLD = 0,
	TX_HOLD_RX_UP,
	TX_DOWN_RX_HOLD,
	TX_HOLD_RX_DOWN
};

/* This is a global spi state flag */
enum trans_status {
	NONE = 0,
	ONE,
	DONE
};

struct Simulator_Control;

struct SPI_Control {  //  Represents an STM32 SPI port, e.g. SPI1, SPI2, SPI3.
  uint8_t id;  //  1=SPI1, 2=SPI2, 3=SPI3.
  uint32_t clock;  //  e.g. 500000 for 500 kHz
  uint8_t bitOrder;  //  e.g. MSBFIRST
  uint8_t dataMode;  //  e.g. SPI_MODE0
  uint32_t timeout;  //  Timeout in milliseconds

  //  Last packet transmitted / received.
  volatile SPI_DATA_TYPE *tx_buf;
  int tx_len;
  volatile SPI_DATA_TYPE *rx_buf;
  int rx_len;
  volatile int rx_buf_remainder;  //  Excess of bytes to be received after transmission.
  volatile trans_status transceive_status;  //  Status of SPI transmit/receive command.

  Evt_t event;  //  Event to signal that replay was completed.
  volatile Evt_t *tx_event;  //  If not NULL, signal this event when transmit has been completed.
  volatile Evt_t *rx_event;  //  If not NULL, signal this event when receive has been completed.
  Simulator_Control *simulator;  //  Simulator for the port.
	SPI_Fails	failCode;   // Last fail code.

  //  STM32 port configuration.
  uint32_t SPIx;
  volatile uint32_t *ptr_SPI_DR;
  volatile uint32_t *ptr_SPI_I2SCFGR;
  uint8_t rx_NVIC_DMA_CHANNEL_IRQ;
  uint8_t tx_NVIC_DMA_CHANNEL_IRQ;

  uint32_t RCC_SPIx;
  uint32_t RCC_GPIOx;
  uint32_t RCC_DMAx;

  uint32_t SS_PORT;   uint16_t SS_PIN;
  uint32_t SCK_PORT;  uint16_t SCK_PIN;
  uint32_t MISO_PORT; uint16_t MISO_PIN;
  uint32_t MOSI_PORT; uint16_t MOSI_PIN;

  uint32_t tx_dma;      //  Transmit DMA Port.
  uint8_t  tx_channel;  //  Transmit DMA Channel.
  uint32_t rx_dma;      //  Receive DMA Port.
  uint8_t  rx_channel;  //  Receive DMA Channel.
};

//  This is the new SPI Interface.  New code should use this.
volatile SPI_Control *spi_setup(uint8_t id);  	//  Enable SPI peripheral and GPIO clocks.  Should be called once only.
SPI_Fails spi_configure(volatile SPI_Control *port, uint32_t clock, uint8_t bitOrder, uint8_t dataMode);
SPI_Fails spi_open(volatile SPI_Control *port);  //  Enable DMA interrupt for SPI1.
//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.
int spi_transceive(volatile SPI_Control *port, volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len);
int spi_transceive_wait(volatile SPI_Control *port, volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len);
volatile Evt_t *spi_transceive_replay(volatile SPI_Control *port);  //  Replay the next transceive request that was captured earlier.
bool spi_is_transceive_completed(volatile SPI_Control *port);  //  Return true if last SPI command was completed.
SPI_Fails spi_wait(volatile SPI_Control *port);  //  Wait until transceive complete.
SPI_Fails spi_close(volatile SPI_Control *port);  //  Disable DMA interrupt for SPI1.
SPI_Fails spi_test(volatile SPI_Control *port);  //  For testing only.
SPI_Fails spi_dump_trail(volatile SPI_Control *port);  //  Dump the captured / replayed / simulated SPI packets to console.
SPI_Fails spi_dump_packet(volatile SPI_Control *port);  //  Dump the last SPI packet to console.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#ifdef __cplusplus  //  SPIInterface class for C++ only
//  This is the legacy SPI Interface for Arduino.  New code should NOT use this.
class SPIInterfaceSettings {
public:
  SPIInterfaceSettings(uint32_t clock, uint8_t bitOrder, uint8_t dataMode);  //  Used by BME280Spi.cpp
  // SPIInterfaceSettings();
  uint8_t spi_port;  //  SPI port for STM32: 1=SPI1, 2=SPI2, 3=SPI3
  uint32_t clock;
  uint8_t bitOrder;
  uint8_t dataMode;
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

  //  From Arduino wiring_digital.h.  We intercept and identify the SPI port to be used.
  static void pinMode(uint8_t, uint8_t);  //  Used by BME280ASpi.h
  static void digitalWrite(uint8_t pin, uint8_t val);  //  Used by BME280ASpi.h
  // static int digitalRead(uint8_t);  //  Used by BME280SpiSw.cpp
};
#endif  //  __cplusplus
#endif  //  SPIINT_H_
