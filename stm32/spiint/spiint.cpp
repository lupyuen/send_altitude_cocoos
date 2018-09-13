//  SPI Interface for STM32 with DMA. Compatible with Arduino's Wire SPI interface.  Based on
//  https://github.com/libopencm3/libopencm3-examples/tree/master/examples/stm32/f1/lisa-m-2/spi_dma_adv
//  https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/tree/master/rtos/oled_dma
//  https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/blob/master/rtos/winbond
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/usart.h>
#include <libopencm3/stm32/dma.h>
#include <libopencm3/cm3/nvic.h>
#include <libopencm3/stm32/spi.h>
#include <string.h>  //  For memcmp()
#include <stdio.h>
#include <errno.h>
#include <logger.h>
#include <bluepill.h>  //  For led_wait()
#include <simulator.h>
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

//  TODO: Update this when running under FreeRTOS.
#define systicks millis
typedef uint32_t TickType_t;

static SPI_DATA_TYPE dummy_tx_buf[MAX_TRAIL_SIZE];  //  TODO

//  Messages for each fail code.
static const char *spi_msg[] = {
	"OK",
	"Missing Simulator",  //  SPI_Missing_Simulator
	"Invalid Port",  //  SPI_Invalid_Port
	"Invalid Size",  //  SPI_Invalid_Size
	"Simulation Mismatch",  //  SPI_Mismatch
	"Read timeout"
};

static volatile SPI_Control allPorts[MAX_SPI_PORTS];  //  Map port ID to the port control.  Volatile because the DMA ISR will lookup this array.

static inline TickType_t diff_ticks(TickType_t early,TickType_t later) {
    //  Compute the difference in ticks.
	if ( later >= early )
		return later - early;
	return ~(TickType_t)0 - early + 1 + later;
}

const char *spi_error(SPI_Fails fcode) {
    //  Return a character string message for SPI_Fails code
	int icode = (int)fcode;
	if ( icode < 0 || icode >= (int)Simulator_End )
		return "Bad SPI_Fails code";
	return spi_msg[icode];
}

static SPI_Fails showError(volatile SPI_Control *port, SPI_Fails fc) {
	if (port) { port->failCode = fc; }
	debug_print("***** Error: SPI Failed ");
	debug_print(fc); debug_print(" / ");
	debug_println(spi_error(fc));
	debug_flush();
	return fc;
}

static const char *get_mode_name(volatile SPI_Control *port) {
	const char *title = "unknown";
	if (!port->simulator) { return title; }
	switch (port->simulator->mode) {
		case Simulator_Capture: title = "captured"; break;
		case Simulator_Replay: title = "replayed"; break;
		case Simulator_Simulate: title = "simulated"; break;
		case Simulator_Mismatch: title = "mismatch"; break;
		default: title = "unknown";
	}
	return title;
}

static void dump_packet(const char *title, volatile SPI_DATA_TYPE *buf, int len) {
	//  Print the contents of the packet.
	debug_print(title); debug_print(" ");
	for (int i = 0; i < len; i++) { debug_printhex(buf[i]); debug_print(" "); } 
}

static void dump_packets(const char *title, volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len) {
	//  Print the contents of the packets.
	debug_print(title); debug_print(" >> ");
	for (int i = 0; i < tx_len; i++) { debug_printhex(tx_buf[i]); debug_print(" "); }
	debug_print("<< ");
	for (int i = 0; i < rx_len; i++) { debug_printhex(rx_buf[i]); debug_print(" "); } 
	debug_println(""); // debug_flush();
}

SPI_Fails spi_dump_packet(volatile SPI_Control *port) {
	//  Dump the last SPI packet to console.
	if (port->simulator == NULL) { return showError(NULL, SPI_Missing_Simulator); }  //  No simulator.
	const char *title = get_mode_name(port);
	dump_packets(title, port->tx_buf, port->tx_len, port->rx_buf, port->rx_len);
	return SPI_Ok;
}

SPI_Fails spi_dump_trail(volatile SPI_Control *port) {
	//  Dump the simulated commands to console.
	if (port->simulator == NULL) { return showError(NULL, SPI_Missing_Simulator); }  //  No simulator.
	const char *title = get_mode_name(port);
	int i = port->simulator->index;
	port->simulator->index = 0;
	debug_print(title); debug_print(" ");
	for (;;) {
		//  Read the captured SPI packet for send and receive.
		int tx_len = simulator_replay_size(port->simulator);
		volatile uint8_t *tx_buf = simulator_replay_packet(port->simulator, tx_len);
		int rx_len = simulator_replay_size(port->simulator);
		volatile uint8_t *rx_buf = simulator_replay_packet(port->simulator, rx_len);
		//  Stop if the packet was invalid.
		if (tx_len < 0 || rx_len < 0 || tx_buf == NULL || rx_buf == NULL) { break; }
		//  Dump the packet.
		dump_packet(">>", tx_buf, tx_len);
		dump_packet("<<", rx_buf, rx_len);
	}
	debug_print("trail index / length: "); debug_print((int) (port->simulator->index));
	debug_print(" / "); debug_println((int) (port->simulator->length)); debug_flush();
	port->simulator->index = i;
	return SPI_Ok;
}

volatile SPI_Control *spi_setup(uint8_t id) {
	debug_println("spi_setup"); debug_flush();
	if (id < 1 || id > MAX_SPI_PORTS) { showError(NULL, SPI_Invalid_Port); return NULL; }

	//  Initialise the ports for the first time.
	static bool firstTime = true;
	if (firstTime) {
		firstTime = false;
		for (int i = 0; i < MAX_SPI_PORTS; i++) { 
			allPorts[i].id = i;
			allPorts[i].tx_dma = 0;
			allPorts[i].tx_channel = 0;
			allPorts[i].rx_dma = 0;
			allPorts[i].rx_channel = 0;
			allPorts[i].event = event_create();
			allPorts[i].tx_event = NULL;
			allPorts[i].rx_event = NULL;
			allPorts[i].simulator = NULL;
		}
	}
	//  Return the port.
	volatile SPI_Control *port = &allPorts[id - 1];
	port->id = id;

	//  Moved to platform_setup() in bluepill.cpp.
	//  rcc_spi_setup_in_hse_12mhz_out_72mhz();

	//  Enable SPI1 Periph and gpio clocks
	rcc_periph_clock_enable(RCC_SPI1);  //  TODO
	rcc_periph_clock_enable(RCC_GPIOA);  //  TODO: Support other ports.

	//  Enable DMA1 clock
	rcc_periph_clock_enable(RCC_DMA1);
	return port;
}

/* TODO: Set up SPI:
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

SPI_Fails spi_configure(
	volatile SPI_Control *port, 
	uint32_t clock, 
	uint8_t bitOrder, 
	uint8_t dataMode) {
	//  if (port->simulator) 
	{ debug_println("spi config"); debug_flush(); }
	port->clock = clock;
	port->bitOrder = bitOrder;
	port->dataMode = dataMode;

  	//  TODO
  	port->rx_dma = DMA1;
	port->rx_channel = DMA_CHANNEL2;
	port->tx_dma = DMA1;
	port->tx_channel = DMA_CHANNEL3;

	port->SPIx = SPI1;
	port->ptr_SPI_DR = (uint32_t) &SPI1_DR;
	port->ptr_SPI_I2SCFGR = &SPI1_I2SCFGR;
	port->rx_NVIC_DMA_CHANNEL_IRQ = NVIC_DMA1_CHANNEL2_IRQ;
	port->tx_NVIC_DMA_CHANNEL_IRQ = NVIC_DMA1_CHANNEL3_IRQ;






	//  Configure GPIOs: SS=PA4, SCK=PA5, MISO=PA6 and MOSI=PA7
	gpio_set_mode(SS_PORT, GPIO_MODE_OUTPUT_50_MHZ,
            GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, SS_PIN);
	gpio_set_mode(SCK_PORT, GPIO_MODE_OUTPUT_50_MHZ,
            GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, SCK_PIN);
	gpio_set_mode(MOSI_PORT, GPIO_MODE_OUTPUT_50_MHZ,
            GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, MOSI_PIN);

	gpio_set_mode(MISO_PORT, GPIO_MODE_INPUT, GPIO_CNF_INPUT_FLOAT,
			MISO_PIN);
	return SPI_Ok;
}

SPI_Fails spi_open(volatile SPI_Control *port) {
	//  Enable DMA interrupt for SPI1.
	//  port->simulator is set in simulator_open().  If not set, that means we shouldn't capture yet e.g. BME280 get module ID at startup.
	//  if (port->simulator == NULL) { debug_println("spi_open no simulator"); debug_flush(); }
	//  if (port->simulator) 
	{ debug_println("spi open"); debug_flush(); }
	port->tx_event = NULL;
	port->rx_event = NULL;
	port->transceive_status = NONE;

	//  Must configure the port every time or replay will fail.
	//  Reset SPI, SPI_CR1 register cleared, SPI is disabled.
	spi_reset(port->SPIx);

	//  Explicitly disable I2S in favour of SPI operation.
	//  SPI1_I2SCFGR = 0;
	if (port->ptr_SPI_I2SCFGR) {
		*(port->ptr_SPI_I2SCFGR) = 0;
	}
	spi_init_master(
		port->SPIx,
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

	//  Enable SPI1 peripheral.
	spi_enable(port->SPIx);
	
	//  SPI1 RX on DMA1 Channel 2
 	nvic_set_priority(port->rx_NVIC_DMA_CHANNEL_IRQ, 0);
	nvic_enable_irq(port->rx_NVIC_DMA_CHANNEL_IRQ);
	//  SPI1 TX on DMA1 Channel 3
	nvic_set_priority(port->tx_NVIC_DMA_CHANNEL_IRQ, 0);
	nvic_enable_irq(port->tx_NVIC_DMA_CHANNEL_IRQ);
	return SPI_Ok;
}

SPI_Fails spi_close(volatile SPI_Control *port) {
	//  Disable DMA interrupt for SPI1.
	if (port->simulator) { debug_println("spi close"); debug_flush(); }

	/* Ensure transceive is complete.
	* This checks the state flag as well as follows the
	* procedure on the Reference Manual (RM0008 rev 14
	* Section 25.3.9 page 692, the note.) */
	//  debug_println("spi_close2"); // debug_flush();
	while (!(SPI_SR(port->SPIx) & SPI_SR_TXE)) {}
	//  debug_println("spi_close3"); // debug_flush();
	while (SPI_SR(port->SPIx) & SPI_SR_BSY) {}

	port->tx_event = NULL;
	port->rx_event = NULL;
 	nvic_disable_irq(port->rx_NVIC_DMA_CHANNEL_IRQ);
 	nvic_disable_irq(port->tx_NVIC_DMA_CHANNEL_IRQ);
	return SPI_Ok;
}

static int spi_simulate_error(volatile SPI_Control *port, SPI_Fails fc, volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len) {
	//  Error while simulating an SPI command.
	//  Verify that tx_len and rx_len are same as captured trail.
	//  Verify that tx_buf is same as captured trail.
	if (port->simulator != NULL) { debug_println("spi_simulate_error"); debug_flush(); }
	//  In case of error, don't simulate.
	showError(NULL, fc);  //  Don't set last fail.
	//  Do an actual transceive, don't simulate.
	if (port->simulator != NULL) {
		port->simulator->mode = Simulator_Mismatch;
	}
	return spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len);
}

static int spi_simulate(volatile SPI_Control *port, volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len) {
	//  Simulate an SPI command.  This means we have just finished the replay, now we simulate to feed the replay SPI data into the driver.
	//  In case of error, don't simulate, perform the actual transceive.
	//  if (port->simulator != NULL) { debug_println("spi sim"); debug_flush(); }
	if (port->simulator == NULL) {
		return spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len);  //  Don't simulate.
	}
	int captured_tx_len = simulator_simulate_size(port->simulator);
	volatile uint8_t *captured_tx_buf = simulator_simulate_packet(port->simulator, captured_tx_len);
	int captured_rx_len = simulator_simulate_size(port->simulator);
	volatile uint8_t *captured_rx_buf = simulator_simulate_packet(port->simulator, captured_rx_len);
	//  Verify that tx_len and rx_len are same as captured trail.
	if (tx_len != captured_tx_len || rx_len != captured_rx_len) {
		return spi_simulate_error(port, SPI_Mismatch, tx_buf, tx_len, rx_buf, rx_len);
	}
	//  Verify that tx_buf is same as captured trail.
	if (memcmp((const void *) tx_buf, (const void *) captured_tx_buf, tx_len) != 0) {
		return spi_simulate_error(port, SPI_Mismatch, tx_buf, tx_len, rx_buf, rx_len);
	}
	//  Copy the captured buffer into rx_buf.
	memcpy((void *) rx_buf, (const void *) captured_rx_buf, rx_len);
	//  Set the status to done so we don't wait.
	port->transceive_status = DONE;
	return 0;  //  No error.
}

static SPI_Fails spi_setup_dma(volatile SPI_Control *port, uint32_t dma, uint8_t channel, volatile SPI_DATA_TYPE *buf, int len, bool set_read_from_peripheral, bool enable_memory_increment_mode) {
	//  Set up DMA for SPI receive and transmit.
	dma_set_peripheral_address(dma, channel, port->ptr_SPI_DR);
	dma_set_memory_address(dma, channel, (uint32_t) buf); // Change here
	dma_set_number_of_data(dma, channel, len); // Change here

	if (set_read_from_peripheral) { dma_set_read_from_peripheral(dma, channel); }  //  For rx: Read from SPI port and write into memory.
	else { dma_set_read_from_memory(dma, channel); }  //  For tx and rx_remainder: Read from memory and write to SPI port.

	if (enable_memory_increment_mode) { dma_enable_memory_increment_mode(dma, channel); }
	else { dma_disable_memory_increment_mode(dma, channel); }  //  For rx_remainder: Always read from the same address, don't increment.

	dma_set_peripheral_size(dma, channel, SPI_PSIZE);
	dma_set_memory_size(dma, channel, SPI_MSIZE);
	dma_set_priority(dma, channel, DMA_CCR_PL_HIGH);
	return SPI_Ok;
}

int spi_transceive(volatile SPI_Control *port, volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len) {	
	//  Send an SPI command to transmit and receive SPI data.  
	//  If the simulator is in...
	//  Capture Mode: We capture the transmit/receive data into the simulator trail.
	//  Replay mode: We replay the transmit/receive SPI command recorded in the simulator trail. Record the received data into the trail.
	//  Simulate mode: We don't execute any SPI commands, just return the data received data from the trail.
	//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.
	//  Return -1 in case of error.
	/////if (port->simulator != NULL) { dump_packet("spi >>", tx_buf, tx_len); }
	//  Check for 0 length in both tx and rx.
	if ((rx_len < 1) && (tx_len < 1)) { showError(port, SPI_Invalid_Size); return -1; }
	//  If this is Simulate Mode, return the data received in Replay Mode.
	if (port->simulator != NULL && port->simulator->mode == Simulator_Simulate) { 
		return spi_simulate(port, tx_buf, tx_len, rx_buf, rx_len); 
	}
	//  Reset DMA channels.
	dma_channel_reset(port->tx_dma, port->tx_channel);
	dma_channel_reset(port->rx_dma, port->rx_channel);

	//  Reset SPI data and status registers. Here we assume that the SPI peripheral is NOT busy any longer, i.e. the last activity was verified complete elsewhere in the program.
	volatile uint8_t temp_data __attribute__ ((unused));
	while (SPI_SR(port->SPIx) & (SPI_SR_RXNE | SPI_SR_OVR)) { temp_data = SPI_DR(port->SPIx); }
	// debug_println("spi_transceive2"); // debug_flush();

	//  Remember the last packet.
	port->tx_buf = tx_buf; port->tx_len = tx_len;
	port->rx_buf = rx_buf; port->rx_len = rx_len;

	//  Reset status flag appropriately (both 0 case caught above).
	port->transceive_status = NONE;
	if (rx_len < 1) { port->transceive_status = ONE; }

	/* Determine tx length case to change behaviour
	 * If tx_len >= rx_len, then normal case, run both DMAs with normal settings
	 * If rx_len == 0, just don't run the rx DMA at all
	 * If tx_len == 0, use a dummy buf and set the tx dma to transfer the same amount as the rx_len, to ensure everything is clocked in
	 * If 0 < tx_len < rx_len, first do a normal case, then on the tx finished interrupt, set up a new dummy buf tx dma transfer for the remaining required clock cycles (handled in tx dma complete interrupt) */
	port->rx_buf_remainder = 0;
	if ((tx_len > 0) && (tx_len < rx_len)) {
		port->rx_buf_remainder = rx_len - tx_len; 
	}
	//  Set up rx dma, note it has higher priority to avoid overrun.
	if (rx_len > 0) {  //  For rx: Read from SPI port and write into memory.
		spi_setup_dma(port, port->rx_dma, port->rx_channel, rx_buf, rx_len, true, true);
	}
	//  Set up tx dma (must always run tx to get clock signal).
	if (tx_len > 0) {  //  Here we have a regular tx transfer.
		//  For tx: Read from memory and write to SPI port.
		spi_setup_dma(port, port->tx_dma, port->tx_channel, tx_buf, tx_len, false, true);
	} else {
		//  TODO: Here we aren't transmitting any real data, use the dummy buffer and set the length to the rx_len to get all rx data in, while not incrementing the memory pointer
		debug_println("WARNING: spi_transceive tx=0"); debug_flush();
		//  For rx_remainder: Read from memory and write to SPI port.
		spi_setup_dma(port, port->tx_dma, port->tx_channel, dummy_tx_buf, rx_len, false, false);
	}

	//  Enable dma transfer complete interrupts.
	//  TODO: Enable other interrupts.
	if (rx_len > 0) { dma_enable_transfer_complete_interrupt(port->rx_dma, port->rx_channel); }
	dma_enable_transfer_complete_interrupt(port->tx_dma, port->tx_channel);

	//  Activate dma channels.
	if (rx_len > 0) { dma_enable_channel(port->rx_dma, port->rx_channel); }
	dma_enable_channel(port->tx_dma, port->tx_channel);

	//  Enable the spi transfer via dma. This will immediately start the transmission, after which when the receive is complete, the receive dma will activate */
	if (rx_len > 0) { spi_enable_rx_dma(port->SPIx); }
    spi_enable_tx_dma(port->SPIx);

	int result = 0;
	// debug_print("spi_transceive returned "); debug_println(result); debug_flush();
    return result;
}

static volatile SPI_Control *findPortByDMA(uint32_t dma, uint8_t channel) {
	//  Called by DMA ISR to return the port by DMA port and channel.  Don't use any external functions.
	for (int i = 0; i < MAX_SPI_PORTS; i++) {
		volatile SPI_Control *port = &allPorts[i];
		if (port->rx_dma == dma && port->rx_channel == channel) { return port; }
		if (port->tx_dma == dma && port->tx_channel == channel) { return port; }
	}
	return NULL;
}

static void update_transceive_status(volatile SPI_Control *port) {
	//  Called by DMA ISR to update the transceive status.  Don't use any external functions.
	if (port == NULL) { return; }
	switch(port->transceive_status) {
		case NONE: port->transceive_status = ONE; break;
		case ONE: port->transceive_status = DONE; break;
		default: break; //  Nothing.
	}
}

volatile SPI_Control *isr_port = NULL;  //  For debug only.
volatile Evt_t *isr_event = NULL;  //  For debug only.

static void handle_rx_interrupt(uint32_t spi, uint32_t dma,  uint8_t channel) {  
	//  Handle SPI receive interrupt on DMA.  Don't call any external functions.
	//  TODO: Handle other errors.
	if ( dma_get_interrupt_flag(dma, channel, DMA_TCIF) )
		{ dma_clear_interrupt_flags(dma, channel, DMA_TCIF); }
	dma_disable_transfer_complete_interrupt(dma, channel);
	spi_disable_rx_dma(spi);
	dma_disable_channel(dma, channel);

	//  Find the port and update the status.
	volatile SPI_Control *port = findPortByDMA(dma, channel);
	isr_port = port; ////
	if (port == NULL) { return; }
	update_transceive_status(port);

	//  For replay: Signal to Sensor Task when receive is done.
	if (port->rx_event != NULL) {
		isr_event = port->rx_event; ////
		event_ISR_signal(*port->rx_event);
	}
}

static void handle_tx_interrupt(uint32_t spi, uint32_t dma,  uint8_t channel) {
	//  Handle SPI transmit interrupt on DMA.  Don't call any external functions.
	//  TODO: Handle other errors.
	if ( dma_get_interrupt_flag(dma, channel, DMA_TCIF) )
		{ dma_clear_interrupt_flags(dma, channel, DMA_TCIF); }
	dma_disable_transfer_complete_interrupt(dma, channel);
	spi_disable_tx_dma(spi);
	dma_disable_channel(dma, channel);

	//  Find the port.
	volatile SPI_Control *port = findPortByDMA(dma, channel);
	if (port == NULL) { return; }

	//  TODO: If tx_len < rx_len, create a dummy transfer to clock in the remaining rx data.
	if (port->rx_buf_remainder > 0) {
		volatile int rx_buf_remainder = port->rx_buf_remainder;
		port->rx_buf_remainder = 0; // Clear the buffer remainder to disable this section later

		dma_channel_reset(port->tx_dma, port->tx_channel);
		spi_setup_dma(port, port->tx_dma, port->tx_channel, dummy_tx_buf, rx_buf_remainder, false, false);

		dma_enable_transfer_complete_interrupt(port->tx_dma, port->tx_channel);
		dma_enable_channel(port->tx_dma, port->tx_channel);
		spi_enable_tx_dma(port->SPIx);
		return;
	} 
	//  Update the status.
	update_transceive_status(port);

	//  Send signal that transmit is done.  (Not used)
	if (port->tx_event != NULL) {
		event_ISR_signal(*port->tx_event);
	}
}

void dma1_channel2_isr(void) {  //  SPI1 receive interrupt on DMA 1 channel 2.
	handle_rx_interrupt(SPI1, DMA1, DMA_CHANNEL2);
}

void dma1_channel3_isr(void) {  //  SPI1 transmit interrupt on DMA 1 channel 3.
	handle_tx_interrupt(SPI1, DMA1, DMA_CHANNEL3);
}

bool spi_is_transceive_completed(volatile SPI_Control *port) {
    //  Return true if last SPI command was completed.
	if (port->transceive_status == DONE) return true;
	return false;
}

SPI_Fails spi_wait(volatile SPI_Control *port) {
	/* Wait until transceive complete.
	* This checks the state flag as well as follows the
	* procedure on the Reference Manual (RM0008 rev 14
	* Section 25.3.9 page 692, the note.) */
	//  TODO: Check for timeout.
	//  debug_println("spi_wait"); // debug_flush();
	while (!spi_is_transceive_completed(port)) {}  //  TODO
	//  debug_println("spi_wait2"); // debug_flush();
	while (!(SPI_SR(port->SPIx) & SPI_SR_TXE)) {}
	//  debug_println("spi_wait3"); // debug_flush();
	while (SPI_SR(port->SPIx) & SPI_SR_BSY) {}
	//  debug_println("spi_wait returned"); // debug_flush();
	return SPI_Ok;
}

int spi_transceive_wait(volatile SPI_Control *port, volatile SPI_DATA_TYPE *tx_buf, int tx_len, volatile SPI_DATA_TYPE *rx_buf, int rx_len) {	
	//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.
	//  Return -1 in case of error.

	//  Start a transceive.
	int result = spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len);
	if (result < 0) { return result; }
	//  Wait for transceive to complete if necessary.
	if (port->simulator != NULL && port->simulator->mode == Simulator_Replay ) {
		//  If simulator is in Replay Mode, don't need to wait now.  Caller will wait for completion event to be signalled.
	} else {
		//  If no simulator, or simulator is in other modes, wait for transceive response.
		spi_wait(port);
		if (port->simulator) { dump_packets(get_mode_name(port), tx_buf, tx_len, rx_buf, rx_len); }
	}
	if (port->simulator != NULL && port->simulator->mode == Simulator_Capture) {
		//  If simulator is in Capture Mode, capture the SPI command for replay and simulation later.
		simulator_capture_size(port->simulator, tx_len);
		simulator_capture_packet(port->simulator, tx_buf, tx_len);
		simulator_capture_size(port->simulator, rx_len);
		simulator_capture_packet(port->simulator, rx_buf, rx_len);
	}
	return result;
}

volatile Evt_t *spi_transceive_replay(volatile SPI_Control *port) {
	//  Replay the next transceive request that was captured earlier.  Return the event for Sensor Task to wait until the request has been completed.
	//  Read the captured SPI packet for send and receive.
	//  if (port->simulator != NULL) { debug_println("spi replay"); debug_flush(); }
	if (port->simulator == NULL) { showError(port, SPI_Missing_Simulator); return NULL; }  //  No simulator.
	int tx_len = simulator_replay_size(port->simulator);
	volatile uint8_t *tx_buf = simulator_replay_packet(port->simulator, tx_len);
	int rx_len = simulator_replay_size(port->simulator);
	volatile uint8_t *rx_buf = simulator_replay_packet(port->simulator, rx_len);
	//  Stop if the packet was invalid.
	if (tx_len < 0 || rx_len < 0 || tx_buf == NULL || rx_buf == NULL) { return NULL; }
	//  Signal when rx_completed.
	volatile Evt_t *event = &port->event;
	port->rx_event = event;
	// dump_packet("replay >>", tx_buf, tx_len); debug_println(""); debug_flush(); debug_print(" rx_event "); debug_println((int) *event); debug_flush();
	isr_port = NULL;  isr_event = NULL;  //  For debugging only

	//  Send the transceive request and signal the event when completed.
	int result = spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len);
	if (result < 0) { return NULL; }
	// for (int i = 0; i < 10; i++) { led_wait(); }
	// debug_print("*** isr_port: "); debug_println(isr_port == NULL ? "NULL" : "OK");
	// debug_print("*** isr_event: "); debug_println(isr_event == NULL ? "NULL" : "OK");

	//  Caller (Sensor Task) must wait for the event to be signaled before sending again.
	return event;
}

//  Which SPI port is now sending/receiving. 1=SPI1, 2=SPI2, ...
static volatile uint8_t currentSPIPort = 0;  //  0=Unknown.

//  Allocate one byte per SPI port for send and receive.
static volatile uint8_t tx_buffer[MAX_SPI_PORTS];  //  Must be static volatile because of DMA.
static volatile uint8_t rx_buffer[MAX_SPI_PORTS];

void SPIInterface::beginTransaction(SPIInterfaceSettings settings) {
	//  Used by BME280Spi.cpp
	if (settings.spi_port < 1 || settings.spi_port > MAX_SPI_PORTS) { showError(NULL, SPI_Invalid_Port); return; }
	currentSPIPort = settings.spi_port;
	volatile SPI_Control *port = &allPorts[currentSPIPort - 1];
	//  SPI setup should have been called in bme280.cpp.  TODO: Verify SPI port number.
	//  spi_setup();
	spi_configure(port, settings.clock, settings.bitOrder, settings.dataMode);
	spi_open(port);
}

uint8_t SPIInterface::transfer(uint8_t data) {
  	//  Send and receive 1 byte of data.  Wait until data is sent and received.  Return the byte received.  Used by BME280Spi.cpp
	if (currentSPIPort < 1 || currentSPIPort > MAX_SPI_PORTS) { showError(NULL, SPI_Invalid_Port); return 0; }
	volatile uint8_t portID = currentSPIPort;  //  Remember in case it changes while waiting.
	volatile SPI_Control *port = &allPorts[portID - 1];

	tx_buffer[portID - 1] = data;
	rx_buffer[portID - 1] = 0x22;  //  Means uninitialised.
	int result = spi_transceive_wait(port, &tx_buffer[portID - 1], 1, &rx_buffer[portID - 1], 1);
	if (result < 0) { return 0; }
	return rx_buffer[portID - 1];
}

void SPIInterface::endTransaction(void) {
	//  Used by BME280Spi.cpp
	if (currentSPIPort < 1 || currentSPIPort > MAX_SPI_PORTS) { showError(NULL, SPI_Invalid_Port); return; }	
	volatile SPI_Control *port = &allPorts[currentSPIPort - 1];
	currentSPIPort = 0;
	spi_close(port);
}

void SPIInterface::pinMode(uint8_t pin, uint8_t mode){
	//  Used by BME280Spi.h
	//  debug_print("pinMode pin "); debug_print((int) pin); debug_print(" mode "); debug_println((int) mode); debug_flush();
}

void SPIInterface::digitalWrite(uint8_t pin, uint8_t val) {
	//  digitalWrite() is called just before an SPI transfer.  We intercept the pin.  Used by BME280Spi.h
	//  debug_print("digitalWrite pin "); debug_print((int) pin);  debug_print(" val "); debug_println((int) val); debug_flush();
	if (pin < 1 || pin > MAX_SPI_PORTS) { showError(NULL, SPI_Invalid_Port); return; }	
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

static volatile uint8_t tx_packet[16];
static volatile uint8_t rx_packet[16];

void spi_test(void) {
	debug_println("spi_test"); debug_flush();

	// bme280 uses the msb to select read and write
	// combine the addr with the read/write bit
	uint8_t addr = ID_ADDR;
	uint8_t readAddr = addr | BME280_SPI_READ;
	
	////    SPI.beginTransaction(SPISettings(500000, MSBFIRST, SPI_MODE0));
	volatile SPI_Control *port = spi_setup(1);

#ifdef DISABLE_DMA
	test_spi_configure();
	for (int i = 0; i < 10; i++) { led_wait(); }
	test_read(readAddr);  //  Should return 96.
#else
	spi_configure(port, 500000, MSBFIRST, SPI_MODE0);
	spi_open(port);

	tx_packet[0] = readAddr;
	rx_packet[0] = 0;
	int tx_len = 1;
	int rx_len = 1;
	spi_transceive_wait(port, tx_packet, tx_len, rx_packet, rx_len);

	// transfer 0x00 to get the data
	tx_packet[0] = 0;
	rx_packet[0] = 0;
	tx_len = 1;
	rx_len = 1;
	spi_transceive_wait(port, tx_packet, tx_len, rx_packet, rx_len);  //  Should return 96.

	spi_close(port);
#endif  //  DISABLE_DMA
	debug_println("spi_test OK"); debug_flush();
}
