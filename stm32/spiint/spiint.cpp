//  SPI Interface for STM32. Compatible with Arduino's Wire SPI interface.  Based on
//  https://github.com/libopencm3/libopencm3-examples/blob/master/examples/stm32/f1/lisa-m-2/spi/spi.c
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/usart.h>
#include <libopencm3/stm32/dma.h>
#include <libopencm3/cm3/nvic.h>
#include <libopencm3/stm32/spi.h>
#include <stdio.h>
#include <errno.h>
#include "spiint.h"

void spi_setup(void) {
	//  Moved to platform_setup() in bluepill.cpp.
	//  rcc_clock_setup_in_hse_12mhz_out_72mhz();

	//  Enable SPI1 peripheral and GPIOA clocks.
	rcc_periph_clock_enable(RCC_GPIOA);  //  GPIOA because SS=PA4, SCK=PA5, MISO=PA6 and MOSI=PA7
	rcc_periph_clock_enable(RCC_SPI1);

	/* Enable GPIOA, GPIOB, GPIOC clock. */
	////rcc_periph_clock_enable(RCC_GPIOB);
	////rcc_periph_clock_enable(RCC_GPIOC);

	/* Enable clocks for GPIO port A (for GPIO_USART2_TX) and USART2. */
	////rcc_periph_clock_enable(RCC_GPIOA);
	////rcc_periph_clock_enable(RCC_AFIO);
	////rcc_periph_clock_enable(RCC_USART2);
}

void spi_configure(void) {
  /* Configure GPIOs: SS=PA4, SCK=PA5, MISO=PA6 and MOSI=PA7 */
  gpio_set_mode(GPIOA, GPIO_MODE_OUTPUT_50_MHZ,
            GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, GPIO4 |  //  SS
            								GPIO5 |  //  SCK
                                            GPIO7 );  //  MOSI

  gpio_set_mode(GPIOA, GPIO_MODE_INPUT, GPIO_CNF_INPUT_FLOAT,
          GPIO6);  //  MISO

  /* Reset SPI, SPI_CR1 register cleared, SPI is disabled */
  spi_reset(SPI1);

  /* Set up SPI in Master mode with:
   * Clock baud rate: 1/64 of peripheral clock frequency
   * Clock polarity: Idle High
   * Clock phase: Data valid on 2nd clock pulse
   * Data frame format: 8-bit
   * Frame format: MSB First
   */
  spi_init_master(SPI1, SPI_CR1_BAUDRATE_FPCLK_DIV_64, SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE,
                  SPI_CR1_CPHA_CLK_TRANSITION_2, SPI_CR1_DFF_8BIT, SPI_CR1_MSBFIRST);

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

static void gpio_setup(void)
{
	/* Set GPIO8 (in GPIO port A) to 'output push-pull'. */
	////gpio_set_mode(GPIOA, GPIO_MODE_OUTPUT_2_MHZ,
		      ////GPIO_CNF_OUTPUT_PUSHPULL, GPIO8);
}

int OLDmain(void)
{
	int counter = 0;
	uint16_t rx_value = 0x42;

	spi_setup();
	gpio_setup();
	////usart_setup();
	spi_configure();

	/* Blink the LED (PA8) on the board with every transmitted byte. */
	while (1) {
		/* LED on/off */
		gpio_toggle(GPIOA, GPIO8);

		/* printf the value that SPI should send */
		printf("Counter: %i  SPI Sent Byte: %i", counter, (uint8_t) counter);
		/* blocking send of the byte out SPI1 */
		spi_send(SPI1, (uint8_t) counter);
		/* Read the byte that just came in (use a loopback between MISO and MOSI
		 * to get the same byte back)
		 */
		rx_value = spi_read(SPI1);
		/* printf the byte just received */
		printf("    SPI Received Byte: %i\r\n", rx_value);

		counter++;
		
	}

	return 0;
}
