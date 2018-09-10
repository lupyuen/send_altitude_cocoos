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

#ifndef USE_16BIT_TRANSFERS
#define USE_16BIT_TRANSFERS 0
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

volatile int transceive_status;

/* Global for dummy tx dma transfer */
int rx_buf_remainder = 0;

#if USE_16BIT_TRANSFERS
uint16_t dummy_tx_buf = 0xdd;
#else
uint8_t dummy_tx_buf = 0xdd;
#endif

void spi_setup(void) {
	//  Moved to platform_setup() in bluepill.cpp.
	//  rcc_spi_setup_in_hse_12mhz_out_72mhz();

	/* Enable SPI1 Periph and gpio clocks */
	rcc_periph_clock_enable(RCC_SPI1);
	rcc_periph_clock_enable(RCC_GPIOA);  //  TODO: Support other ports.

	/* Enable DMA1 clock */
	rcc_periph_clock_enable(RCC_DMA1);

	/* Enable GPIOA, GPIOB, GPIOC clock. */
	////rcc_periph_clock_enable(RCC_GPIOB);
	////rcc_periph_clock_enable(RCC_GPIOC);
}

void spi_configure(void) {

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
	 * Clock baud rate: 1/256 of SPI1 peripheral clock frequency (281.25 kHz)
	 * Clock polarity: Idle Low
	 * Clock phase: Data valid on 1st clock pulse
	 * Data frame format: 8-bit or 16-bit
	 * Frame format: MSB First
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
	 */
#define SPI_MODE0 0x00
#define SPI_MODE1 0x04
#define SPI_MODE2 0x08
#define SPI_MODE3 0x0C

#if USE_16BIT_TRANSFERS
	spi_init_master(
		SPI1,
		SPI_CR1_BAUDRATE_FPCLK_DIV_256, ////
		SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE, ////
		SPI_CR1_CPHA_CLK_TRANSITION_1, ////
		SPI_CR1_DFF_16BIT,
		SPI_CR1_MSBFIRST
	);
#else
	spi_init_master(
		SPI1,
		SPI_CR1_BAUDRATE_FPCLK_DIV_256, ////
		SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE, ////
		SPI_CR1_CPHA_CLK_TRANSITION_1, ////
		SPI_CR1_DFF_8BIT,
		SPI_CR1_MSBFIRST
	);
#endif

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

	/* Enable SPI1 periph. */
	spi_enable(SPI1);
}

void spi_open(void) {
	//  Enable DMA interrupt for SPI1.
	/* SPI1 RX on DMA1 Channel 2 */
 	nvic_set_priority(NVIC_DMA1_CHANNEL2_IRQ, 0);
	nvic_enable_irq(NVIC_DMA1_CHANNEL2_IRQ);
	/* SPI1 TX on DMA1 Channel 3 */
	nvic_set_priority(NVIC_DMA1_CHANNEL3_IRQ, 0);
	nvic_enable_irq(NVIC_DMA1_CHANNEL3_IRQ);
}

void spi_close(void) {
	//  Disable DMA interrupt for SPI1.
 	nvic_disable_irq(NVIC_DMA1_CHANNEL2_IRQ);
 	nvic_disable_irq(NVIC_DMA1_CHANNEL3_IRQ);
}

#if USE_16BIT_TRANSFERS
int spi_transceive(uint16_t *tx_buf, int tx_len, uint16_t *rx_buf, int rx_len)
#else
int spi_transceive(uint8_t *tx_buf, int tx_len, uint8_t *rx_buf, int rx_len)
#endif
{	

	/* Check for 0 length in both tx and rx */
	if ((rx_len < 1) && (tx_len < 1)) {
		/* return -1 as error */
		return -1;
	}

	/* Reset DMA channels*/
	dma_channel_reset(DMA1, DMA_CHANNEL2);
	dma_channel_reset(DMA1, DMA_CHANNEL3);

	/* Reset SPI data and status registers.
	 * Here we assume that the SPI peripheral is NOT
	 * busy any longer, i.e. the last activity was verified
	 * complete elsewhere in the program.
	 */
	volatile uint8_t temp_data __attribute__ ((unused));
	while (SPI_SR(SPI1) & (SPI_SR_RXNE | SPI_SR_OVR)) {
		temp_data = SPI_DR(SPI1);
	}

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
#if USE_16BIT_TRANSFERS
		dma_set_peripheral_size(DMA1, DMA_CHANNEL2, DMA_CCR_PSIZE_16BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL2, DMA_CCR_MSIZE_16BIT);
#else
		dma_set_peripheral_size(DMA1, DMA_CHANNEL2, DMA_CCR_PSIZE_8BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL2, DMA_CCR_MSIZE_8BIT);
#endif
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
#if USE_16BIT_TRANSFERS
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, DMA_CCR_PSIZE_16BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, DMA_CCR_MSIZE_16BIT);
#else
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, DMA_CCR_PSIZE_8BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, DMA_CCR_MSIZE_8BIT);
#endif
		dma_set_priority(DMA1, DMA_CHANNEL3, DMA_CCR_PL_HIGH);
	} else {
		/* Here we aren't transmitting any real data, use the dummy buffer
		 * and set the length to the rx_len to get all rx data in, while
		 * not incrementing the memory pointer
		 */
		dma_set_peripheral_address(DMA1, DMA_CHANNEL3, (uint32_t)&SPI1_DR);
		dma_set_memory_address(DMA1, DMA_CHANNEL3, (uint32_t)(&dummy_tx_buf)); // Change here
		dma_set_number_of_data(DMA1, DMA_CHANNEL3, rx_len); // Change here
		dma_set_read_from_memory(DMA1, DMA_CHANNEL3);
		dma_disable_memory_increment_mode(DMA1, DMA_CHANNEL3); // Change here
#if USE_16BIT_TRANSFERS
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, DMA_CCR_PSIZE_16BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, DMA_CCR_MSIZE_16BIT);
#else
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, DMA_CCR_PSIZE_8BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, DMA_CCR_MSIZE_8BIT);
#endif
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

    return 0;
}

/* SPI receive completed with DMA */
void dma1_channel2_isr(void)
{
	//// TODO: gpio_set(SS_PORT, SS_PIN);
	//// TODO: gpio_clear(SS_PORT, SS_PIN);
	if ((DMA1_ISR &DMA_ISR_TCIF2) != 0) {
		DMA1_IFCR |= DMA_IFCR_CTCIF2;
	}

	dma_disable_transfer_complete_interrupt(DMA1, DMA_CHANNEL2);

	spi_disable_rx_dma(SPI1);

	dma_disable_channel(DMA1, DMA_CHANNEL2);

	/* Increment the status to indicate one of the transfers is complete */
	transceive_status++;
	//// TODO: gpio_clear(SS_PORT, SS_PIN);
	//// TODO: gpio_set(SS_PORT, SS_PIN);
}

/* SPI transmit completed with DMA */
void dma1_channel3_isr(void)
{
	////gpio_set(GPIOB, GPIO1);
	if ((DMA1_ISR &DMA_ISR_TCIF3) != 0) {
		DMA1_IFCR |= DMA_IFCR_CTCIF3;
	}

	dma_disable_transfer_complete_interrupt(DMA1, DMA_CHANNEL3);

	spi_disable_tx_dma(SPI1);

	dma_disable_channel(DMA1, DMA_CHANNEL3);

	/* If tx_len < rx_len, create a dummy transfer to clock in the remaining
	 * rx data
	 */
	if (rx_buf_remainder > 0) {
		dma_channel_reset(DMA1, DMA_CHANNEL3);
		dma_set_peripheral_address(DMA1, DMA_CHANNEL3, (uint32_t)&SPI1_DR);
		dma_set_memory_address(DMA1, DMA_CHANNEL3, (uint32_t)(&dummy_tx_buf)); // Change here
		dma_set_number_of_data(DMA1, DMA_CHANNEL3, rx_buf_remainder); // Change here
		dma_set_read_from_memory(DMA1, DMA_CHANNEL3);
		dma_disable_memory_increment_mode(DMA1, DMA_CHANNEL3); // Change here
#if USE_16BIT_TRANSFERS
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, DMA_CCR_PSIZE_16BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, DMA_CCR_MSIZE_16BIT);
#else
		dma_set_peripheral_size(DMA1, DMA_CHANNEL3, DMA_CCR_PSIZE_8BIT);
		dma_set_memory_size(DMA1, DMA_CHANNEL3, DMA_CCR_MSIZE_8BIT);
#endif
		dma_set_priority(DMA1, DMA_CHANNEL3, DMA_CCR_PL_HIGH);

		rx_buf_remainder = 0; // Clear the buffer remainder to disable this section later

		dma_enable_transfer_complete_interrupt(DMA1, DMA_CHANNEL3);
		dma_enable_channel(DMA1, DMA_CHANNEL3);
		spi_enable_tx_dma(SPI1);
	} else {
		/* Increment the status to indicate one of the transfers is complete */
		transceive_status++;
	}

	////gpio_clear(GPIOB, GPIO1);
}

static void OLDgpio_setup(void)
{
	/* Set LED_PIN (in GPIO port A) to 'output push-pull'. */
	gpio_set_mode(GPIOA, GPIO_MODE_OUTPUT_2_MHZ,
		      GPIO_CNF_OUTPUT_PUSHPULL, LED_PIN);

	/* Use the extra pins to signal when the ISRs are running */
	/* First, SPI1 - SS pin on Lisa/M v2.0 */
	gpio_set_mode(GPIOA, GPIO_MODE_OUTPUT_2_MHZ,
		      GPIO_CNF_OUTPUT_PUSHPULL, SS_PIN);
	/* Then, SPI1 - DRDY pin on Lisa/M v2.0 */
	gpio_set_mode(GPIOB, GPIO_MODE_OUTPUT_2_MHZ,
		      GPIO_CNF_OUTPUT_PUSHPULL, GPIO1);

}

void spi_test(void)
{
	debug_println("spi_test"); debug_flush();
	int counter_tx = 0;
	int counter_rx = 0;
	cnt_state counter_state = TX_UP_RX_HOLD;
	int i = 0;
	transceive_status = DONE;

	////    SPI.beginTransaction(SPISettings(500000, MSBFIRST, SPI_MODE0));
	spi_setup();
	// gpio_setup();
	// usart_setup();
	spi_configure();
	spi_open();
	// dma_setup();

	// bme280 uses the msb to select read and write
	// combine the addr with the read/write bit
	static const uint8_t ID_ADDR         = 0xD0;
	static const uint8_t BME280_SPI_WRITE   = 0x7F;
   	static const uint8_t BME280_SPI_READ    = 0x80;
	uint8_t addr = ID_ADDR;
	uint8_t readAddr = addr | BME280_SPI_READ;
	uint8_t tx_packet[1] = { readAddr };
	uint8_t rx_packet[1] = { 0 };
	int tx_len = 1;
	int rx_len = 1;

	/* Blink the LED (PA8) on the board with every transmitted byte. */
	{
		/* LED on/off */
		// gpio_toggle(GPIOA, LED_PIN);

		/* Print what is going to be sent on the SPI bus */
		debug_print("Sending  packet tx len "); debug_println(counter_tx);
		for (i = 0; i < tx_len; i++)
		{
			debug_print((int) tx_packet[i]); debug_print(" "); 
		}
		debug_println(""); debug_flush();

		/* Start a transceive */
		if (spi_transceive(tx_packet, tx_len, rx_packet, rx_len)) {
			debug_println("Attempted 0 length tx and rx packets"); debug_flush();
		}

		/* Wait until transceive complete.
		 * This checks the state flag as well as follows the
		 * procedure on the Reference Manual (RM0008 rev 14
		 * Section 25.3.9 page 692, the note.)
		 */
		while (transceive_status != DONE)
			;
		while (!(SPI_SR(SPI1) & SPI_SR_TXE))
			;
		while (SPI_SR(SPI1) & SPI_SR_BSY)
			;

		/* Print what was received on the SPI bus */
		debug_print("Received Packet rx len"); debug_println(rx_len);
		for (i = 0; i < rx_len; i++) {
			debug_print(rx_packet[i]); debug_print(" ");
		}
		debug_println(""); debug_flush();

		/* Update counters
		 * If we use the loopback method, we can not
		 * have a rx length longer than the tx length.
		 * Testing rx lengths longer than tx lengths
		 * requires an actual slave device that will
		 * return data.
		 */
		switch (counter_state) {
			case TX_UP_RX_HOLD:
				counter_tx++;
				if (counter_tx > 15) {
					counter_state = TX_HOLD_RX_UP;
				}
				break;
			case TX_HOLD_RX_UP:
				counter_rx++;
				if (counter_rx > 15) {
					counter_state = TX_DOWN_RX_HOLD;
				}
				break;
			case TX_DOWN_RX_HOLD:
				counter_tx--;
				if (counter_tx < 1) {
					counter_state = TX_HOLD_RX_DOWN;
				}
				break;
			case TX_HOLD_RX_DOWN:
				counter_rx--;
				if (counter_rx < 1) {
					counter_state = TX_UP_RX_HOLD;
				}
				break;
			default:
				;
		}
	}

	spi_close();
}
