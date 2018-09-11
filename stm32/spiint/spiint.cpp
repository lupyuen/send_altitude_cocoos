//  SPI Interface for STM32 with DMA. Compatible with Arduino's Wire SPI interface.  Based on
//  https://github.com/libopencm3/libopencm3-examples/tree/master/examples/stm32/f1/lisa-m-2/spi_dma_adv
//  https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/tree/master/rtos/oled_dma
//  https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/blob/master/rtos/winbond

/*
 * This file is part of the libopencm3 project.
 *
 * Copyright (C) 2009 Uwe Hermann <uwe@hermann-uwe.de>
 * Copyright (C) 2013 Stephen Dwyer <scdwyer@ualberta.ca>
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/usart.h>
#include <libopencm3/stm32/dma.h>
#include <libopencm3/cm3/nvic.h>
#include <libopencm3/stm32/spi.h>
#include <stdio.h>
#include <errno.h>
#include <logger.h>
#include "bluepill.h"  //  For led_wait()
#include "spiint.h"

#ifndef USE_16BIT_SPI_TRANSFERS
#define USE_16BIT_SPI_TRANSFERS 0
#endif

#if USE_16BIT_SPI_TRANSFERS
#define SPI_PSIZE DMA_CCR_PSIZE_16BIT
#define SPI_MSIZE DMA_CCR_MSIZE_16BIT
#define SPI_DFF SPI_CR1_DFF_16BIT
#else
#define SPI_PSIZE DMA_CCR_PSIZE_8BIT
#define SPI_MSIZE DMA_CCR_MSIZE_8BIT
#define SPI_DFF SPI_CR1_DFF_8BIT
#endif

//  Configure GPIOs: SS=PA4, SCK=PA5, MISO=PA6 and MOSI=PA7.  TODO: Support other ports and pins.
#define SS_PORT GPIOA
#define SS_PIN GPIO4
#define SCK_PORT GPIOA
#define SCK_PIN GPIO5
#define MISO_PORT GPIOA
#define MISO_PIN GPIO6
#define MOSI_PORT GPIOA
#define MOSI_PIN GPIO7

////
#define LED_PIN GPIO8

/* This is for the counter state flag */
typedef enum {
	TX_UP_RX_HOLD = 0,
	TX_HOLD_RX_UP,
	TX_DOWN_RX_HOLD,
	TX_HOLD_RX_DOWN
} cnt_state;

/* This is a global spi state flag */
typedef enum {
	NONE = 0,
	ONE,
	DONE
} trans_status;

volatile int transceive_status;  //  TODO: Allocate per port.

/* Global for dummy tx dma transfer */
int rx_buf_remainder = 0;  //  TODO: Allocate per port.

SPI_DATA_TYPE dummy_tx_buf = 0xdd;

void spi_setup(void) {
	debug_println("spi_setup"); debug_flush();

	//  Moved to platform_setup() in bluepill.cpp.
	//  rcc_spi_setup_in_hse_12mhz_out_72mhz();

	/* Enable SPI1 Periph and gpio clocks */
	rcc_periph_clock_enable(RCC_SPI1);
	rcc_periph_clock_enable(RCC_GPIOA);  //  TODO: Support other ports.

	/* Enable DMA1 clock */
	rcc_periph_clock_enable(RCC_DMA1);
}

void spi_configure(uint32_t clock, uint8_t bitOrder, uint8_t dataMode) {
	//  debug_println("spi_configure"); debug_flush();

	//  Configure GPIOs: SS=PA4, SCK=PA5, MISO=PA6 and MOSI=PA7
	gpio_set_mode(SS_PORT, GPIO_MODE_OUTPUT_50_MHZ,
            GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, SS_PIN);
	gpio_set_mode(SCK_PORT, GPIO_MODE_OUTPUT_50_MHZ,
            GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, SCK_PIN);
	gpio_set_mode(MOSI_PORT, GPIO_MODE_OUTPUT_50_MHZ,
            GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, MOSI_PIN);

	gpio_set_mode(MISO_PORT, GPIO_MODE_INPUT, GPIO_CNF_INPUT_FLOAT,
			MISO_PIN);

	/* Reset SPI, SPI_CR1 register cleared, SPI is disabled */
	spi_reset(SPI1);

	/* Explicitly disable I2S in favour of SPI operation */
	SPI1_I2SCFGR = 0;

	/* Set up SPI in Master mode with:
	SPI Mode
	Clock Polarity
	Clock Phase
	0 =
	SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE
	SPI_CR1_CPHA_CLK_TRANSITION_1
	1 =
	SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE
	SPI_CR1_CPHA_CLK_TRANSITION_2
	2 =
	SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE
	SPI_CR1_CPHA_CLK_TRANSITION_1
	3 =
	SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE
	SPI_CR1_CPHA_CLK_TRANSITION_2
	”
	Excerpt From: Warren Gay. “Beginning STM32.”

	“Divisor
	Macro
	SPI1 Frequency
	SPI2 Frequency
	2
	SPI_CR1_BAUDRATE_FPCLK_DIV_2 =
	36 MHz
	18 MHz
	4
	SPI_CR1_BAUDRATE_FPCLK_DIV_4 =
	18 MHz
	9 MHz
	8
	SPI_CR1_BAUDRATE_FPCLK_DIV_8 =
	9 MHz
	4.5 MHz
	16
	SPI_CR1_BAUDRATE_FPCLK_DIV_16 =
	4.5 MHz
	2.25 MHz
	32
	SPI_CR1_BAUDRATE_FPCLK_DIV_32 =
	2.25 MHz
	1.125 MHz
	64
	SPI_CR1_BAUDRATE_FPCLK_DIV_64 =
	1.125 MHz
	562.5 kHz
	128
	SPI_CR1_BAUDRATE_FPCLK_DIV_128 =
	562.5 kHz
	281.25 kHz
	256
	SPI_CR1_BAUDRATE_FPCLK_DIV_256 =
	281.25 kHz
	140.625 kHz”

	Excerpt From: Warren Gay. “Beginning STM32.” iBooks. 
	 */
#define SPI_MODE0 0x00
#define SPI_MODE1 0x04
#define SPI_MODE2 0x08
#define SPI_MODE3 0x0C

	spi_init_master(
		SPI1,
		SPI_CR1_BAUDRATE_FPCLK_DIV_256, ////  SPI1 at 281.25 kHz
		// SPI_CR1_BAUDRATE_FPCLK_DIV_128, ////  SPI1 at 562.5 kHz

		//  SPI_MODE0:
		//  SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE, ////
		//  SPI_CR1_CPHA_CLK_TRANSITION_1, ////

		//  SPI_MODE3:
		SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE,
		SPI_CR1_CPHA_CLK_TRANSITION_2,

		SPI_DFF,
		SPI_CR1_MSBFIRST
	);

#define HARDWARE_NSS //  Software NSS doesn't work.
#ifdef HARDWARE_NSS
	//  Set NSS management to hardware.
	//  Important!  	You must have a pullup resistor on the NSS
 	//  line in order that the NSS (/CS) SPI output
 	//  functions correctly as a chip select. The
 	//  SPI peripheral configures NSS pin as an
 	//  open drain output.
	//  debug_println("spi_configure hardware nss"); debug_flush();
	spi_disable_software_slave_management(SPI1);
	spi_enable_ss_output(SPI1);
#else
	/*
	 * Set NSS management to software.
	 *
	 * Note:
	 * Setting nss high is very important, even if we are controlling the GPIO
	 * ourselves this bit needs to be at least set to 1, otherwise the spi
	 * peripheral will not send any data out.
	 */
	spi_enable_software_slave_management(SPI1);
	spi_set_nss_high(SPI1);
#endif  //  NSS_HARDWARE

	/* Enable SPI1 periph. */
	spi_enable(SPI1);
}

void spi_open(void) {
	//  Enable DMA interrupt for SPI1.
	//  debug_println("spi_open"); debug_flush();
	/* SPI1 RX on DMA1 Channel 2 */
 	nvic_set_priority(NVIC_DMA1_CHANNEL2_IRQ, 0);
	nvic_enable_irq(NVIC_DMA1_CHANNEL2_IRQ);
	/* SPI1 TX on DMA1 Channel 3 */
	nvic_set_priority(NVIC_DMA1_CHANNEL3_IRQ, 0);
	nvic_enable_irq(NVIC_DMA1_CHANNEL3_IRQ);
}

void spi_close(void) {
	//  Disable DMA interrupt for SPI1.
	//  debug_println("spi_close"); debug_flush();
 	nvic_disable_irq(NVIC_DMA1_CHANNEL2_IRQ);
 	nvic_disable_irq(NVIC_DMA1_CHANNEL3_IRQ);
}

int spi_transceive(volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len) {	
	//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.
	//  Return -1 in case of error.

	//  Print what is going to be sent on the SPI bus
	//  debug_print("spi_transceive sending len "); debug_println(tx_len);
	//  for (int i = 0; i < tx_len; i++) { debug_print((int) tx_buf[i]); debug_print(" "); } debug_println(""); debug_flush();

	/* Check for 0 length in both tx and rx */
	if ((rx_len < 1) && (tx_len < 1)) {
		/* return -1 as error */
		debug_println("***** ERROR: Attempted 0 length SPI transceive"); debug_flush();
		return -1;
	}
	/* Reset DMA channels*/
	dma_channel_reset(DMA1, DMA_CHANNEL2);
	dma_channel_reset(DMA1, DMA_CHANNEL3);
	// debug_println("spi_transceive1"); // debug_flush();

	/* Reset SPI data and status registers.
	 * Here we assume that the SPI peripheral is NOT
	 * busy any longer, i.e. the last activity was verified
	 * complete elsewhere in the program.
	 */
	volatile uint8_t temp_data __attribute__ ((unused));
	while (SPI_SR(SPI1) & (SPI_SR_RXNE | SPI_SR_OVR)) {
		temp_data = SPI_DR(SPI1);
	}
	// debug_println("spi_transceive2"); // debug_flush();

	/* Reset status flag appropriately (both 0 case caught above) */
	transceive_status = NONE;
	if (rx_len < 1) {
		transceive_status = ONE;
	}

	/* Determine tx length case to change behaviour
	 * If tx_len >= rx_len, then normal case, run both DMAs with normal settings
	 * If rx_len == 0, just don't run the rx DMA at all
	 * If tx_len == 0, use a dummy buf and set the tx dma to transfer the same
	 *    amount as the rx_len, to ensure everything is clocked in
	 * If 0 < tx_len < rx_len, first do a normal case, then on the tx finished
	 *    interrupt, set up a new dummyy buf tx dma transfer for the remaining
	 *    required clock cycles (handled in tx dma complete interrupt)
	 */
	rx_buf_remainder = 0;
	if ((tx_len > 0) && (tx_len < rx_len)) {
		rx_buf_remainder = rx_len - tx_len;
	}

	/* Set up rx dma, note it has higher priority to avoid overrun */
	if (rx_len > 0) {
		dma_set_peripheral_address(DMA1, DMA_CHANNEL2, (uint32_t)&SPI1_DR);
		dma_set_memory_address(DMA1, DMA_CHANNEL2, (uint32_t)rx_buf);
		dma_set_number_of_data(DMA1, DMA_CHANNEL2, rx_len);
		dma_set_read_from_peripheral(DMA1, DMA_CHANNEL2);
		dma_enable_memory_increment_mode(DMA1, DMA_CHANNEL2);
		dma_set_peripheral_size(DMA1, DMA_CHANNEL2, SPI_PSIZE);
		dma_set_memory_size(DMA1, DMA_CHANNEL2, SPI_MSIZE);
		dma_set_priority(DMA1, DMA_CHANNEL2, DMA_CCR_PL_VERY_HIGH);
	}

	/* Set up tx dma (must always run tx to get clock signal) */
	if (tx_len > 0) {
		/* Here we have a regular tx transfer */
		dma_set_peripheral_address(DMA1, DMA_CHANNEL3, (uint32_t)&SPI1_DR);
		dma_set_memory_address(DMA1, DMA_CHANNEL3, (uint32_t)tx_buf);
		dma_set_number_of_data(DMA1, DMA_CHANNEL3, tx_len);
		dma_set_read_from_memory(DMA1, DMA_CHANNEL3);
		dma_enable_memory_increment_mode(DMA1, DMA_CHANNEL3);
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, SPI_PSIZE);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, SPI_MSIZE);
		dma_set_priority(DMA1, DMA_CHANNEL3, DMA_CCR_PL_HIGH);
	} else {
		/* TODO: Here we aren't transmitting any real data, use the dummy buffer
		 * and set the length to the rx_len to get all rx data in, while
		 * not incrementing the memory pointer
		 */
		debug_println("spi_transceive tx=0"); debug_flush();
		dma_set_peripheral_address(DMA1, DMA_CHANNEL3, (uint32_t)&SPI1_DR);
		dma_set_memory_address(DMA1, DMA_CHANNEL3, (uint32_t)(&dummy_tx_buf)); // Change here
		dma_set_number_of_data(DMA1, DMA_CHANNEL3, rx_len); // Change here
		dma_set_read_from_memory(DMA1, DMA_CHANNEL3);
		dma_disable_memory_increment_mode(DMA1, DMA_CHANNEL3); // Change here
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, SPI_PSIZE);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, SPI_MSIZE);
		dma_set_priority(DMA1, DMA_CHANNEL3, DMA_CCR_PL_HIGH);
	}

	/* Enable dma transfer complete interrupts */
	if (rx_len > 0) {
		dma_enable_transfer_complete_interrupt(DMA1, DMA_CHANNEL2);
	}
	dma_enable_transfer_complete_interrupt(DMA1, DMA_CHANNEL3);

	/* Activate dma channels */
	if (rx_len > 0) {
		dma_enable_channel(DMA1, DMA_CHANNEL2);
	}
	dma_enable_channel(DMA1, DMA_CHANNEL3);

	/* Enable the spi transfer via dma
	 * This will immediately start the transmission,
	 * after which when the receive is complete, the
	 * receive dma will activate
	 */
	if (rx_len > 0) {
    	spi_enable_rx_dma(SPI1);
    }
    spi_enable_tx_dma(SPI1);

	int result = 0;
	// debug_print("spi_transceive returned "); debug_println(result); debug_flush();
    return result;
}

/* SPI receive completed with DMA */
void dma1_channel2_isr(void)
{
	if ( dma_get_interrupt_flag(DMA1, DMA_CHANNEL2, DMA_TCIF) )
		{ dma_clear_interrupt_flags(DMA1, DMA_CHANNEL2, DMA_TCIF); }
	dma_disable_transfer_complete_interrupt(DMA1, DMA_CHANNEL2);
	spi_disable_rx_dma(SPI1);
	dma_disable_channel(DMA1, DMA_CHANNEL2);

	/* Increment the status to indicate one of the transfers is complete */
	transceive_status++;
}

/* SPI transmit completed with DMA */
void dma1_channel3_isr(void) {
	if ( dma_get_interrupt_flag(DMA1, DMA_CHANNEL3, DMA_TCIF) )
		{ dma_clear_interrupt_flags(DMA1, DMA_CHANNEL3, DMA_TCIF); }
	dma_disable_transfer_complete_interrupt(DMA1, DMA_CHANNEL3);
	spi_disable_tx_dma(SPI1);
	dma_disable_channel(DMA1, DMA_CHANNEL3);
	/* TODO: If tx_len < rx_len, create a dummy transfer to clock in the remaining
	 * rx data */
	if (rx_buf_remainder > 0) {
		dma_channel_reset(DMA1, DMA_CHANNEL3);
		dma_set_peripheral_address(DMA1, DMA_CHANNEL3, (uint32_t)&SPI1_DR);
		dma_set_memory_address(DMA1, DMA_CHANNEL3, (uint32_t)(&dummy_tx_buf)); // Change here
		dma_set_number_of_data(DMA1, DMA_CHANNEL3, rx_buf_remainder); // Change here
		dma_set_read_from_memory(DMA1, DMA_CHANNEL3);
		dma_disable_memory_increment_mode(DMA1, DMA_CHANNEL3); // Change here
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, SPI_PSIZE);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, SPI_MSIZE);
		dma_set_priority(DMA1, DMA_CHANNEL3, DMA_CCR_PL_HIGH);

		rx_buf_remainder = 0; // Clear the buffer remainder to disable this section later

		dma_enable_transfer_complete_interrupt(DMA1, DMA_CHANNEL3);
		dma_enable_channel(DMA1, DMA_CHANNEL3);
		spi_enable_tx_dma(SPI1);
	} else {
		/* Increment the status to indicate one of the transfers is complete */
		transceive_status++;
	}
}

void spi_wait(void) {
	/* Wait until transceive complete.
	* This checks the state flag as well as follows the
	* procedure on the Reference Manual (RM0008 rev 14
	* Section 25.3.9 page 692, the note.)
	*/
	//  TODO: Check for timeout.
	//  debug_println("spi_wait"); // debug_flush();
	while (transceive_status != DONE) {}
	//  debug_println("spi_wait2"); // debug_flush();
	while (!(SPI_SR(SPI1) & SPI_SR_TXE)) {}
	//  debug_println("spi_wait3"); // debug_flush();
	while (SPI_SR(SPI1) & SPI_SR_BSY) {}
	//  debug_println("spi_wait returned"); // debug_flush();
}

int spi_transceive_wait(volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len) {	
	//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.
	//  Return -1 in case of error.

	//  Start a transceive.
	transceive_status = DONE;  //  TODO: Status per SPI port.
	int result = spi_transceive(tx_buf, tx_len, rx_buf, rx_len);
	if (result < 0) { return result; }

	//  Wait until transceive complete.
	spi_wait();

	//  Print what was received on the SPI bus.
	//  debug_print("spi_transceive_wait received len "); debug_println(rx_len);
	//  for (int i = 0; i < rx_len; i++) { debug_print(rx_buf[i]); debug_print(" "); } debug_println(""); debug_flush();
	return result;
}

//  Which SPI port is now sending/receiving. 1=SPI1, 2=SPI2, ...
static volatile uint8_t currentSPIPort = 0;  //  0=Unknown.

//  Allocate one byte per SPI port for send and receive, including the unknown port (0).
static volatile uint8_t tx_buffer[MAX_SPI_PORTS + 1];  //  Must be static volatile because of DMA.
static volatile uint8_t rx_buffer[MAX_SPI_PORTS + 1];

static volatile uint8_t tx_packet[16];
static volatile uint8_t rx_packet[16];

void SPIInterface::beginTransaction(SPIInterfaceSettings settings) {
	//  Used by BME280Spi.cpp
	if (settings.spi_port < 1 || settings.spi_port > MAX_SPI_PORTS) {
		debug_print("***** ERROR: SPIInterface.beginTransaction invalid SPI port "); debug_println((int) settings.spi_port); debug_flush();
		return;
	}
	currentSPIPort = settings.spi_port;
	//  SPI setup should have been called in bme280.cpp.  TODO: Verify SPI port number.
	//  spi_setup();
	spi_configure(settings.clock, settings.bitOrder, settings.dataMode);
	spi_open();
}

uint8_t SPIInterface::transfer(uint8_t data) {
  	//  Send and receive 1 byte of data.  Wait until data is sent and received.  Return the byte received.  Used by BME280Spi.cpp
	if (currentSPIPort < 1 || currentSPIPort > MAX_SPI_PORTS) {
		debug_print("***** ERROR: SPIInterface.transfer invalid SPI port "); debug_println((int) currentSPIPort); debug_flush();
		return 0;
	}
	volatile uint8_t port = currentSPIPort;  //  Remember in case it changes while waiting.
	tx_buffer[port] = data;
	rx_buffer[port] = 0x22;  //  Means uninitialised.
	int result = spi_transceive_wait(&tx_buffer[port], 1, &rx_buffer[port], 1);
	if (result < 0) { return 0; }
	return rx_buffer[port];
}

void SPIInterface::endTransaction(void) {
	//  Used by BME280Spi.cpp
	if (currentSPIPort < 1 || currentSPIPort > MAX_SPI_PORTS) {
		debug_print("***** ERROR: SPIInterface.endTransaction invalid SPI port "); debug_println((int) currentSPIPort); debug_flush();
		return;
	}
	currentSPIPort = 0;
	spi_close();
}

void SPIInterface::pinMode(uint8_t pin, uint8_t mode){
	//  Used by BME280Spi.h
	//  debug_print("pinMode pin "); debug_print((int) pin); debug_print(" mode "); debug_println((int) mode); debug_flush();
}

void SPIInterface::digitalWrite(uint8_t pin, uint8_t val) {
	//  digitalWrite() is called just before an SPI transfer.  We intercept the pin.  Used by BME280Spi.h
	//  debug_print("digitalWrite pin "); debug_print((int) pin);  debug_print(" val "); debug_println((int) val); debug_flush();
	if (pin < 1 || pin > MAX_SPI_PORTS) {
		debug_print("***** ERROR: SPIInterface.digitalWrite Invalid SPI port "); debug_println((int) currentSPIPort); debug_flush();
		return;
	}
	currentSPIPort = pin;
}

SPIInterfaceSettings::SPIInterfaceSettings(uint32_t clock0, uint8_t bitOrder0, uint8_t dataMode0) {
	//  Used by BME280Spi.cpp
	spi_port = 1;  //  Default to SPI1.
	clock = clock0;
	bitOrder = bitOrder0;
	dataMode = dataMode0;
}

static const uint8_t ID_ADDR         = 0xD0;
static const uint8_t BME280_SPI_WRITE   = 0x7F;
static const uint8_t BME280_SPI_READ    = 0x80;

// #define DISABLE_DMA
#ifdef DISABLE_DMA

static void
test_spi_configure(void) {
	debug_println("test_spi_configure"); // debug_flush();
	rcc_periph_clock_enable(RCC_SPI1);
	gpio_set_mode(
		GPIOA,
        GPIO_MODE_OUTPUT_50_MHZ,
        GPIO_CNF_OUTPUT_ALTFN_PUSHPULL,
        GPIO4|GPIO5|GPIO7		// NSS=PA4,SCK=PA5,MOSI=PA7
	);
	gpio_set_mode(
		GPIOA,
		GPIO_MODE_INPUT,
		GPIO_CNF_INPUT_FLOAT,
		GPIO6				// MISO=PA6
	);
	spi_reset(SPI1); 
	spi_init_master(
		SPI1,
        SPI_CR1_BAUDRATE_FPCLK_DIV_256,
        SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE,
		SPI_CR1_CPHA_CLK_TRANSITION_1,
	    SPI_CR1_DFF_8BIT,
	    SPI_CR1_MSBFIRST
	);
#ifdef HARDWARE_NSS
	//  Set NSS management to hardware.
	//  Important!  	You must have a pullup resistor on the NSS
 	//  line in order that the NSS (/CS) SPI output
 	//  functions correctly as a chip select. The
 	//  SPI peripheral configures NSS pin as an
 	//  open drain output.
	debug_println("test_spi_configure hardware nss"); debug_flush();
	spi_disable_software_slave_management(SPI1);
	spi_enable_ss_output(SPI1);
#else
	/*
	 * Set NSS management to software.
	 *
	 * Note:
	 * Setting nss high is very important, even if we are controlling the GPIO
	 * ourselves this bit needs to be at least set to 1, otherwise the spi
	 * peripheral will not send any data out.
	 */
	spi_enable_software_slave_management(SPI1);
	spi_set_nss_high(SPI1);

	//  Set SS to high (NSS low) to select SPI interface of BME280 instead of I2C.
	//  gpio_clear(SS_PORT, SS_PIN);
	//  gpio_set(SS_PORT, SS_PIN);

#endif  //  NSS_HARDWARE
}

static uint8_t
test_read(uint8_t readAddr) {
	debug_print("test_read addr "); debug_println((int) readAddr); debug_flush();
	spi_enable(SPI1);

#ifndef HARDWARE_NSS
	//  Assume SS is high.  Before reading, set SS to low (NSS high).
	//  gpio_set(SS_PORT, SS_PIN);
	gpio_clear(SS_PORT, SS_PIN);
#endif  //  !HARDWARE_NSS	

#define DUMMY 0x00
	spi_xfer(SPI1, readAddr);
	uint8_t sr1 = spi_xfer(SPI1, DUMMY);

#ifndef HARDWARE_NSS
	//  After reading, set SS to high (NSS low).
	//  gpio_clear(SS_PORT, SS_PIN);
	gpio_set(SS_PORT, SS_PIN);
#endif  //  !HARDWARE_NSS	

	spi_disable(SPI1);
	debug_print("test_read received "); debug_println((int) sr1); debug_flush();
	return sr1;
}
#endif  //  DISABLE_DMA

void spi_test(void) {
	debug_println("spi_test"); debug_flush();

	// bme280 uses the msb to select read and write
	// combine the addr with the read/write bit
	uint8_t addr = ID_ADDR;
	uint8_t readAddr = addr | BME280_SPI_READ;
	
	////    SPI.beginTransaction(SPISettings(500000, MSBFIRST, SPI_MODE0));
	spi_setup();

#ifdef DISABLE_DMA
	test_spi_configure();
	for (int i = 0; i < 10; i++) { led_wait(); }
	test_read(readAddr);  //  Should return 96.
#else
	spi_configure(500000, MSBFIRST, SPI_MODE0);
	spi_open();

	tx_packet[0] = readAddr;
	rx_packet[0] = 0;
	int tx_len = 1;
	int rx_len = 1;
	spi_transceive_wait(tx_packet, tx_len, rx_packet, rx_len);

	// transfer 0x00 to get the data
	tx_packet[0] = 0;
	rx_packet[0] = 0;
	tx_len = 1;
	rx_len = 1;
	spi_transceive_wait(tx_packet, tx_len, rx_packet, rx_len);  //  Should return 96.

	spi_close();
#endif  //  DISABLE_DMA
	debug_println("spi_test OK"); debug_flush();
}
