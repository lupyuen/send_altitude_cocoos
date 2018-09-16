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
#include <bluepill.h>   //  For led_wait()
#include <simulator.h>  //  For Simulator that captures, replays and simulates SPI commands
#include "spiint.h"

//  TODO: Update this when running under FreeRTOS.
#define systicks millis
typedef uint32_t TickType_t;

static SPI_Fails spi_simulate(SPI_Control *port, SPI_DATA_TYPE *tx_buf, int tx_len, SPI_DATA_TYPE *rx_buf, int rx_len);
static SPI_Fails spi_setup_dma(SPI_Control *port, uint32_t dma, uint8_t channel, SPI_DATA_TYPE *buf, int len, bool set_read_from_peripheral, bool enable_memory_increment_mode);
static void enable_interrupts(uint32_t dma, uint8_t channel);
static void disable_interrupts(uint32_t dma, uint8_t channel);
static void update_transceive_status(SPI_Control *port, Trans_Status new_status = (Trans_Status) -1);
static uint32_t get_baudrate(SPI_Control *port);
static uint32_t get_frequency(SPI_Control *port);
static uint32_t get_clock_polarity(SPI_Control *port);
static uint32_t get_clock_phase(SPI_Control *port);
static uint32_t get_bit_order(SPI_Control *port);
static void dump_config(SPI_Control *port);
static void dump_packets(const char *title, SPI_DATA_TYPE *tx_buf, int tx_len, SPI_DATA_TYPE *rx_buf, int rx_len);
static void dump_packet(const char *title, SPI_DATA_TYPE *buf, int len);
static void dump_history(SPI_Control *port);
static inline TickType_t diff_ticks(TickType_t early, TickType_t later);
static SPI_Fails showError(SPI_Control *port, const char *title, SPI_Fails fc);
static const char *get_mode_name(SPI_Control *port);

static SPI_DATA_TYPE dummy_tx_buf[MAX_TRAIL_SIZE];
static SPI_Control allPorts[MAX_SPI_PORTS];  //  Map port ID to the port control.  Volatile because the DMA ISR will lookup this array.

//////////////////////////////////////////////////////////////////////////
//  SPI Transceive Operations

SPI_Fails spi_transceive(
	SPI_Control *port, 		    //  SPI port.
	SPI_DATA_TYPE *tx_buf, 	    //  Bytes to be transmitted to SPI port.	Must be in static memory, not stack.
	int tx_len, 			    //  Number of bytes to be transmitted.
	SPI_DATA_TYPE *rx_buf,      //  Buffer for receiving bytes from SPI port.
	int rx_len, 			    //  Number of bytes to be received. Must be in static memory, not stack.
	Sem_t *completed_semaphore  //  If not null, semaphore to be signalled upon completing the request.
	) {	
	//  Send an SPI command to transmit and receive SPI data via DMA and interrupts.  If the simulator is in...
	//  Capture Mode: We capture the transmit/receive data into the simulator trail.
	//  Replay mode: We replay the transmit/receive SPI command recorded in the simulator trail. Record the received data into the trail.
	//  Simulate mode: We don't execute any SPI commands, just return the data received data from the trail.
	//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.
	//  if (port->simulator != NULL) { dump_packet("spi transceive >>", tx_buf, tx_len); }

	//  Check for 0 length in both tx and rx.
	if ((rx_len < 1) && (tx_len < 1)) { return showError(port, "spi_transceive", SPI_Invalid_Size); }
	//  If this is Simulate Mode, return the data received in Replay Mode.
	if (port->simulator != NULL && port->simulator->mode == Simulator_Simulate) { 
		return spi_simulate(port, tx_buf, tx_len, rx_buf, rx_len); 
	}
	//  Reset DMA channels.
	dma_channel_reset(port->tx_dma, port->tx_channel);
	dma_channel_reset(port->rx_dma, port->rx_channel);

	//  Reset SPI data and status registers. Here we assume that the SPI peripheral is NOT busy any longer, i.e. the last activity was verified complete elsewhere in the program.
	volatile uint8_t temp_data __attribute__ ((unused));  //  Must be declared volatile or compiler may optimise away the code.
	while (SPI_SR(port->SPIx) & (SPI_SR_RXNE | SPI_SR_OVR)) { temp_data = SPI_DR(port->SPIx); }

	//  Remember the last packet.
	port->tx_buf = tx_buf; port->tx_len = tx_len;
	port->rx_buf = rx_buf; port->rx_len = rx_len;

	//  Signal this semaphore when receive is completed.
	port->rx_semaphore = completed_semaphore;
	port->tx_semaphore = NULL;

	//  Reset status flag appropriately (both 0 case caught above).
	port->transceive_status = TRANS_NONE;
	if (rx_len < 1) { port->transceive_status = TRANS_TX_COMPLETE; }
	port->transceive_history[0] = (Trans_Status) 0;  //  Clear the history.

	/* Determine tx length case to change behaviour
	 * If tx_len >= rx_len, then normal case, run both DMAs with normal settings
	 * If rx_len == 0, just don't run the rx DMA at all
	 * If tx_len == 0, use a dummy buf and set the tx dma to transfer the same amount as the rx_len, to ensure everything is clocked in
	 * If 0 < tx_len < rx_len, first do a normal case, then on the tx finished interrupt, set up a new dummy buf tx dma transfer for the remaining required clock cycles (handled in tx dma complete interrupt) */
	port->rx_remainder = 0;
	if ((tx_len > 0) && (tx_len < rx_len)) { port->rx_remainder = rx_len - tx_len; }

	//  Set up rx DMA, note it has higher priority to avoid overrun.  Read from SPI port and write into memory.
	if (rx_len > 0) { spi_setup_dma(port, port->rx_dma, port->rx_channel, rx_buf, rx_len, true, true); }

	//  Set up tx DMA (must always run tx to get clock signal). Read from memory and write to SPI port.
	if (tx_len > 0) {  //  Here we have a regular tx transfer.
		spi_setup_dma(port, port->tx_dma, port->tx_channel, tx_buf, tx_len, false, true);
	} else {  //  TODO: Here we aren't transmitting any real data, use the dummy buffer and set the length to the rx_len to get all rx data in, while not incrementing the memory pointer
		debug_println("WARNING: spi_transceive tx=0"); debug_flush();
		spi_setup_dma(port, port->tx_dma, port->tx_channel, dummy_tx_buf, rx_len, false, false);
	}

	//  Enable DMA receive complete, error and half-done interrupts.
	if (rx_len > 0) { enable_interrupts(port->rx_dma, port->rx_channel);  }
	//  Enable DMA transmit complete, error and half-done interrupts.
	enable_interrupts(port->tx_dma, port->tx_channel);

	//  Activate DMA channels.
	if (rx_len > 0) { dma_enable_channel(port->rx_dma, port->rx_channel); }
	dma_enable_channel(port->tx_dma, port->tx_channel);

	//  Enable the SPI transfer via DMA. This will immediately start the transmission, after which when the receive is complete, the receive DMA will activate.
	if (rx_len > 0) { spi_enable_rx_dma(port->SPIx); }
    spi_enable_tx_dma(port->SPIx);
	return SPI_Ok;
}

SPI_Fails spi_transceive_wait(SPI_Control *port, SPI_DATA_TYPE *tx_buf, int tx_len, SPI_DATA_TYPE *rx_buf, int rx_len) {	
	//  This function blocks until the result is received.  Should only be used for legacy Arduino code.
	//  New code should call spi_transceive() and pass a semaphore to be signalled.
	//  Note: tx_buf and rx_buf MUST be buffers in static memory, not on the stack.

	//  Start a transceive.
	SPI_Fails result = spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len, NULL);
	if (result != SPI_Ok) { return result; }
	//  Wait for transceive to complete if necessary.
	if (port->simulator != NULL && port->simulator->mode == Simulator_Replay ) {
		//  If simulator is in Replay Mode, don't need to wait now.  Caller will wait for completion semaphore to be signalled.
	} else {
		//  If no simulator, or simulator is in other modes, wait for transceive response.
		spi_wait(port);  //  dump_history(port);
		if (port->simulator) { dump_packets(get_mode_name(port), tx_buf, tx_len, rx_buf, rx_len); }
	}
	if (port->simulator != NULL && port->simulator->mode == Simulator_Capture) {
		//  If simulator is in Capture Mode, capture the SPI command for replay and simulation later.
		simulator_capture_size(port->simulator, tx_len);
		simulator_capture_packet(port->simulator, tx_buf, tx_len);
		simulator_capture_size(port->simulator, rx_len);
		simulator_capture_packet(port->simulator, rx_buf, rx_len);
		simulator_merge_packet(port->simulator, tx_buf, tx_len, rx_buf, rx_len);
	}
	return SPI_Ok;
}

SPI_Fails spi_wait(SPI_Control *port) {
	//  Wait until transceive complete, or when timeout is reached. This function blocks and should be avoided in multitasking programs.
	//  This checks the state flag as well as follows the procedure on the Reference Manual (RM0008 rev 14 Section 25.3.9 page 692, the note.)
	//  debug_println("spi_wait"); // debug_flush();
	TickType_t startTime = systicks();
	while (!spi_is_transceive_complete(port)) {
		if (diff_ticks(startTime, systicks()) > port->timeout)
			{ update_transceive_status(port, TRANS_TIMEOUT); return showError(port, "spi_wait", SPI_Timeout); }
	}
	while (!(SPI_SR(port->SPIx) & SPI_SR_TXE)) {
		if (diff_ticks(startTime, systicks()) > port->timeout)
			{ update_transceive_status(port, TRANS_TIMEOUT); return showError(port, "spi_wait", SPI_Timeout); }
	}
	while (SPI_SR(port->SPIx) & SPI_SR_BSY) {
		if (diff_ticks(startTime, systicks()) > port->timeout)
			{ update_transceive_status(port, TRANS_TIMEOUT); return showError(port, "spi_wait", SPI_Timeout); }
	}
	return SPI_Ok;
}

bool spi_is_transceive_complete(SPI_Control *port) {
    //  Return true if last SPI command was completed successfully or with error.
	volatile Trans_Status status = port->transceive_status;
	if (status == TRANS_RX_COMPLETE
		|| status == TRANS_TIMEOUT
		|| status == TRANS_TX_ERROR
		|| status == TRANS_RX_ERROR) {
		return true;
	}
	// volatile Trans_Status status2 = port->transceive_status; debug_print("spi complete "); debug_print((int) status); debug_print(" / "); debug_print((int) status2); debug_print(" / "); debug_print((int) port->transceive_status); debug_print(" "); dump_history(port);
	return false;
}

bool spi_is_transceive_successful(SPI_Control *port) {
    //  Return true if last SPI command was completed successfully.
	volatile Trans_Status status = port->transceive_status;
	if (status == TRANS_RX_COMPLETE) {
		return true;
	}
	return false;
}

//////////////////////////////////////////////////////////////////////////
//  SPI Port Operations

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

SPI_Fails spi_open(SPI_Control *port) {
	//  Enable DMA interrupt for SPI port.
	//  port->simulator is set in simulator_open().  If not set, that means we shouldn't capture yet e.g. BME280 get module ID at startup.
	//  if (port->simulator == NULL) { debug_println("spi_open no simulator"); debug_flush(); }
	//  if (port->simulator) 
	{ debug_print("spi open spi"); debug_println((int) port->id); dump_config(port); debug_flush(); }
	port->tx_semaphore = NULL;
	port->rx_semaphore = NULL;
	port->transceive_status = TRANS_NONE;

	//  Note: Must configure the port every time or replay will fail.
	//  Reset SPI, SPI_CR1 register cleared, SPI is disabled.
	spi_reset(port->SPIx);

	//  Explicitly disable I2S in favour of SPI operation, i.e. SPI1_I2SCFGR = 0
	if (port->ptr_SPI_I2SCFGR) { *(port->ptr_SPI_I2SCFGR) = 0; }

	//  Set the SPI port baudrate, clock polarity, clock phase and bit order.
	uint32_t baudrate = get_baudrate(port);  	   		 //  e.g. SPI_CR1_BAUDRATE_FPCLK_DIV_256
	uint32_t clock_polarity = get_clock_polarity(port);  //  e.g. SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE
	uint32_t clock_phase = get_clock_phase(port);     	 //  e.g. SPI_CR1_CPHA_CLK_TRANSITION_1
	uint32_t bit_order = get_bit_order(port);			 //  e.g. SPI_CR1_MSBFIRST
	spi_init_master(port->SPIx, baudrate, clock_polarity, clock_phase, SPI_DFF, bit_order);

#define HARDWARE_NSS //  Software NSS doesn't work.
#ifdef HARDWARE_NSS
	//  Set NSS management to hardware. Important! You must have a pullup resistor on the NSS line in order that the NSS (/CS) SPI output
 	//  functions correctly as a chip select. The SPI peripheral configures NSS pin as an open drain output.
	spi_disable_software_slave_management(port->SPIx);
	spi_enable_ss_output(port->SPIx);
#else
	//  Set NSS management to software. Note: Setting nss high is very important, even if we are controlling the GPIO
	//  ourselves this bit needs to be at least set to 1, otherwise the spi peripheral will not send any data out.
	spi_enable_software_slave_management(port->SPIx);
	spi_set_nss_high(port->SPIx);
#endif  //  NSS_HARDWARE

	//  Enable SPI peripheral.
	spi_enable(port->SPIx);
	
	//  Enable interrupts on SPI RX DMA channel.
 	nvic_set_priority(port->rx_irq, 0);
	nvic_enable_irq(port->rx_irq);

	//  Enable interrupts on SPI TX DMA channel.
	nvic_set_priority(port->tx_irq, 0);
	nvic_enable_irq(port->tx_irq);
	return SPI_Ok;
}

SPI_Fails spi_close(SPI_Control *port) {
	//  Close the SPI port.  Disable DMA interrupts for SPI port.
	if (port->simulator) { debug_print("spi close spi"); debug_println((int) port->id); debug_flush(); }
	port->tx_semaphore = NULL;
	port->rx_semaphore = NULL;

	//  Ensure transceive is complete.  This checks the state flag as well as follows the procedure on the Reference Manual (RM0008 rev 14 Section 25.3.9 page 692, the note.)
	TickType_t startTime = systicks();
	while (!(SPI_SR(port->SPIx) & SPI_SR_TXE)) {
		if (diff_ticks(startTime, systicks()) > port->timeout)
			{ break; }
	}
	while (SPI_SR(port->SPIx) & SPI_SR_BSY) {
		if (diff_ticks(startTime, systicks()) > port->timeout)
			{ break; }
	}

	//  Disable interrupts.
 	nvic_disable_irq(port->rx_irq);
 	nvic_disable_irq(port->tx_irq);
	return SPI_Ok;
}

//////////////////////////////////////////////////////////////////////////
//  SPI Command Simulation

static SPI_Fails spi_simulate_error(SPI_Control *port, SPI_Fails fc, SPI_DATA_TYPE *tx_buf, int tx_len, SPI_DATA_TYPE *rx_buf, int rx_len);

SPI_Fails spi_transceive_replay(SPI_Control *port, Sem_t *completed_semaphore) {
	//  Replay the next transceive request that was captured earlier.  If completed_semaphore is non-NULL,
	//  signal the semaphore when the request has been completed.
	//  if (port->simulator != NULL) { debug_println("spi replay"); debug_flush(); }
	if (port->simulator == NULL) { return showError(port, "spi_transceive_replay", SPI_Missing_Simulator); }  //  No simulator.
	int tx_len = 0; uint8_t *tx_buf = NULL; int rx_len = 0; uint8_t *rx_buf = NULL;

	//  If packet merging is enabled, transmit the merged trail in a single burst.
	if (simulator_should_replay_merged_trail(port->simulator)) {
		tx_len = port->simulator->merged_length;
		rx_len = port->simulator->merged_length;
		tx_buf = port->simulator->merged_tx;
		rx_buf = port->simulator->merged_rx;
		// dump_packet("replay merge >>", tx_buf, tx_len); debug_println(""); debug_flush();
	} else {
		//  Read the next captured SPI packet for send and receive.
		tx_len = simulator_replay_size(port->simulator);
		tx_buf = simulator_replay_packet(port->simulator, tx_len);
		rx_len = simulator_replay_size(port->simulator);
		rx_buf = simulator_replay_packet(port->simulator, rx_len);
		//  Stop if the packet was invalid.
		if (tx_len < 0 || rx_len < 0 || tx_buf == NULL || rx_buf == NULL) { return showError(port, "spi_transceive_replay", SPI_Mismatch); }
		// dump_packet("replay >>", tx_buf, tx_len); debug_println(""); debug_flush(); debug_print(" rx_sem "); debug_println((int) completed_semaphore ? *completed_semaphore : -1); debug_flush();
	}
	//  Send the transceive request and signal the semaphore when completed.
	SPI_Fails result = spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len, completed_semaphore);
	if (result != SPI_Ok) { return result; }

	//  Caller (Sensor Task) must wait for the semaphore to be signaled before sending again.
	return SPI_Ok;
}

SPI_Fails spi_split_trail(SPI_Control *port) {
	//  Split the received merged packet into the simulator trail.
	//  debug_print("spi before split: "); spi_dump_trail(port); debug_flush();
	uint8_t *received_packet = port->simulator->merged_rx;
	uint8_t received_length = port->simulator->merged_length;
	uint8_t received_index = 0;
	for (;;) {
		//  Read the next captured SPI packet for send and receive.
		int tx_len = simulator_replay_size(port->simulator);
		uint8_t *tx_buf = simulator_replay_packet(port->simulator, tx_len);
		int rx_len = simulator_replay_size(port->simulator);
		uint8_t *rx_buf = simulator_replay_packet(port->simulator, rx_len);
		//  Stop if no more packets.
		if (tx_len < 0 || rx_len < 0 || tx_buf == NULL || rx_buf == NULL) { break; }
		//  Copy the received packet into the replay packet.
		if ((received_index + rx_len) >= (received_length + 1)) { return showError(port, "spi_split_trail", SPI_Mismatch); }
		memcpy((void *) rx_buf, (const void *) &received_packet[received_index], rx_len);
		received_index = received_index + rx_len;
	}
	//  debug_print("spi after split: "); spi_dump_trail(port); debug_flush();
	return SPI_Ok;
}

static SPI_Fails spi_simulate(SPI_Control *port, SPI_DATA_TYPE *tx_buf, int tx_len, SPI_DATA_TYPE *rx_buf, int rx_len) {
	//  Simulate an SPI command.  This means we have just finished the replay, now we simulate to feed the replay SPI data into the driver.
	//  In case of error, don't simulate, perform the actual transceive.
	//  if (port->simulator != NULL) { debug_println("spi sim"); debug_flush(); }
	if (port->simulator == NULL) {
		return spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len, NULL);  //  Don't simulate.
	}
	//  Read the next transceive packet from the captured trail.
	int captured_tx_len = simulator_simulate_size(port->simulator);
	uint8_t *captured_tx_buf = simulator_simulate_packet(port->simulator, captured_tx_len);
	int captured_rx_len = simulator_simulate_size(port->simulator);
	uint8_t *captured_rx_buf = simulator_simulate_packet(port->simulator, captured_rx_len);
	//  Verify that tx_len and rx_len are same as transceive packet from the captured trail.
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
	port->transceive_status = TRANS_RX_COMPLETE;
	return SPI_Ok;  //  No error.
}

static SPI_Fails spi_simulate_error(SPI_Control *port, SPI_Fails fc, SPI_DATA_TYPE *tx_buf, int tx_len, SPI_DATA_TYPE *rx_buf, int rx_len) {
	//  Error while simulating an SPI command.
	//  Verify that tx_len and rx_len are same as captured trail.
	//  Verify that tx_buf is same as captured trail.
	if (port->simulator != NULL) { debug_println("spi_simulate_error"); debug_flush(); }
	//  In case of error, don't simulate.
	showError(NULL, "spi_simulate", fc);  //  Don't set last fail.
	//  Do an actual transceive, don't simulate.
	if (port->simulator != NULL) {
		port->simulator->mode = Simulator_Mismatch;
	}
	return spi_transceive(port, tx_buf, tx_len, rx_buf, rx_len, NULL);
}

//////////////////////////////////////////////////////////////////////////
//  SPI Port Configuration

SPI_Fails spi_configure(
	SPI_Control *port, 
	uint32_t speedMaximum, 
	uint8_t bitOrder, 
	uint8_t dataMode) {
	//  if (port->simulator) 
	{ debug_print("spi config spi"); debug_println((int) port->id); debug_flush(); }
	port->speedMaximum = speedMaximum;
	port->bitOrder = bitOrder;
	port->dataMode = dataMode;
	port->timeout = 2000;  //  Timeout is 2 seconds.

	//  Configure output pins for SPI: SS, SCK, MOSI.  Set to the fastest mode (50MHz) so that output changes will be immediate.
	gpio_set_mode(port->ss_port, GPIO_MODE_OUTPUT_50_MHZ,
        GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, port->ss_pin);
	gpio_set_mode(port->sck_port, GPIO_MODE_OUTPUT_50_MHZ,
        GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, port->sck_pin);
	gpio_set_mode(port->mosi_port, GPIO_MODE_OUTPUT_50_MHZ,
        GPIO_CNF_OUTPUT_ALTFN_PUSHPULL, port->mosi_pin);
	//  Configure input pin for SPI: MISO.
	gpio_set_mode(port->miso_port, GPIO_MODE_INPUT, GPIO_CNF_INPUT_FLOAT,
		port->miso_pin);
	return SPI_Ok;
}

static SPI_Fails spi_setup_dma(SPI_Control *port, uint32_t dma, uint8_t channel, SPI_DATA_TYPE *buf, int len, bool set_read_from_peripheral, bool enable_memory_increment_mode) {
	//  Set up DMA for SPI receive and transmit.
	dma_set_peripheral_address(dma, channel, (uint32_t) port->ptr_SPI_DR);  //  SPI port for the DMA transfer.
	dma_set_memory_address(dma, channel, (uint32_t) buf);  //  Set the memory buffer.  Must be in static memory, not stack.
	dma_set_number_of_data(dma, channel, len);  //  Set the number of bytes to transmit/receive.

	if (set_read_from_peripheral) { dma_set_read_from_peripheral(dma, channel); }  //  For rx: Read from SPI port and write into memory.
	else { dma_set_read_from_memory(dma, channel); }  //  For tx and rx_remainder: Read from memory and write to SPI port.

	//  Increment the memory buffer pointer so that the entire buffer will be transmitted/received.
	if (enable_memory_increment_mode) { dma_enable_memory_increment_mode(dma, channel); }
	else { dma_disable_memory_increment_mode(dma, channel); }  //  For rx_remainder: Always read from the same address, don't increment.

	dma_set_peripheral_size(dma, channel, SPI_PSIZE);  //  Select 8 or 16-bit SPI transfer.
	dma_set_memory_size(dma, channel, SPI_MSIZE);      //  Select 8 or 16-bit memory transfer.
	dma_set_priority(dma, channel, DMA_CCR_PL_HIGH);   //  Set DMA priority.
	return SPI_Ok;
}

#define Baudrate_Col 0
#define SPI1_Frequency_Col 1  //  Same as SPI ID
#define SPI2_Frequency_Col 2  //  Same as SPI ID

const uint32_t baudrateDivisors[][3] = {
	//  Divisor Macro, 				  SPI1 Frequency,             SPI2 Frequency
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_2,   (uint32_t) (36 * MHz), 	  (uint32_t) (18 * MHz) },
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_4,   (uint32_t) (18 * MHz), 	  (uint32_t) (9 * MHz) },
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_8,   (uint32_t) (9 * MHz), 	  (uint32_t) (4.5 * MHz) },
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_16,  (uint32_t) (4.5 * MHz), 	  (uint32_t) (2.25 * MHz) },
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_32,  (uint32_t) (2.25 * MHz), 	  (uint32_t) (1.125 * MHz) },
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_64,  (uint32_t) (1.125 * MHz),   (uint32_t) (562.5 * kHz) },
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_128, (uint32_t) (562.5 * kHz),   (uint32_t) (281.25 * kHz) },
	{ SPI_CR1_BAUDRATE_FPCLK_DIV_256, (uint32_t) (281.25 * kHz),  (uint32_t) (140.625 * kHz) },
	{ 0, 0, 0 }  //  Last row.
};

static const uint32_t *match_baudrate(SPI_Control *port) {
	//  Return the SPI baudrate row given the max speed in the port settings.
	const uint32_t *row = baudrateDivisors[0];
	for (int i = 0; baudrateDivisors[i][SPI1_Frequency_Col] > 0; i++) {
		//  Get the frequency for the SPI port.
		row = baudrateDivisors[i];
		const uint32_t freq = row[port->id];  //  SPI1=Col 1, SPI2=Col 2
		if (port->speedMaximum >= freq) { return row; }
	}
	return row;  //  No match. Return the lowest baudrate.
}

static uint32_t get_baudrate(SPI_Control *port) {
	//  Return the SPI baudrate given the max speed in the port settings.
	const uint32_t *row = match_baudrate(port);
	const uint32_t baudrate = row[Baudrate_Col];
	return baudrate;
}

static uint32_t get_frequency(SPI_Control *port) {
	//  Return the SPI frequency given the max speed in the port settings.
	const uint32_t *row = match_baudrate(port);
	const uint32_t freq = row[port->id];  //  SPI1=Col 1, SPI2=Col 2
	return freq;
}

static uint32_t get_clock_polarity(SPI_Control *port) {
	//  Return the SPI Clock Polarity (CPOL) given the SPI mode in the port settings.
	switch (port->dataMode) {
		case SPI_MODE0: return SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE;
		case SPI_MODE1: return SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE;
		case SPI_MODE2: return SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE;
		case SPI_MODE3: return SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE;
		default: return SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE;
	}
}

static uint32_t get_clock_phase(SPI_Control *port) {
	//  Return the SPI Clock Phase (CPHA) given the SPI mode in the port settings.
	switch (port->dataMode) {
		case SPI_MODE0: return SPI_CR1_CPHA_CLK_TRANSITION_1;
		case SPI_MODE1: return SPI_CR1_CPHA_CLK_TRANSITION_2;
		case SPI_MODE2: return SPI_CR1_CPHA_CLK_TRANSITION_1;
		case SPI_MODE3: return SPI_CR1_CPHA_CLK_TRANSITION_2;
		default: return SPI_CR1_CPHA_CLK_TRANSITION_1;
	}
}

static uint32_t get_bit_order(SPI_Control *port) {
	//  Return the SPI bit order given the SPI port settings.
	if (port->bitOrder == LSBFIRST) { return SPI_CR1_LSBFIRST; }
	return SPI_CR1_MSBFIRST;
}

//////////////////////////////////////////////////////////////////////////
//  SPI Port Definitions

//  _set(SPIx) becomes port->SPIx = SPIx;
#define _set(x) port->x = x

static SPI_Fails spi_init_port(
	uint8_t id,
	uint32_t SPIx,  					 //  SPI Port e.g. SPI1
	volatile uint32_t *ptr_SPI_DR,  	 //  SPI DR e.g. &SPI1_DR
	volatile uint32_t *ptr_SPI_I2SCFGR,  //  SPI I2C Config e.g. &SPI1_I2SCFGR
	rcc_periph_clken RCC_SPIx,			 //  SPI Clock e.g. RCC_SPI1

	//  GPIO config (port, pin, clock) for each SPI pin (SS, SCK, MISO, MOSI)
	uint32_t ss_port,   uint16_t ss_pin,   rcc_periph_clken ss_rcc,    //  SS pin e.g. GPIOA, GPIO4, RCC_GPIOA
	uint32_t sck_port,  uint16_t sck_pin,  rcc_periph_clken sck_rcc,   //  SCK pin e.g. GPIOA, GPIO5, RCC_GPIOA
	uint32_t miso_port, uint16_t miso_pin, rcc_periph_clken miso_rcc,  //  MISO pin e.g. GPIOA, GPIO6, RCC_GPIOA
	uint32_t mosi_port, uint16_t mosi_pin, rcc_periph_clken mosi_rcc,  //  MOSI pin e.g. GPIOA, GPIO7, RCC_GPIOA

	//  DMA config (port, channel, interrupt, clock) for transmit and receive DMA channels.
	uint32_t tx_dma, uint8_t tx_channel, uint8_t tx_irq, rcc_periph_clken tx_rcc,  //  Transmit DMA e.g. DMA1, DMA_CHANNEL3, NVIC_DMA1_CHANNEL3_IRQ, RCC_DMA1
	uint32_t rx_dma, uint8_t rx_channel, uint8_t rx_irq, rcc_periph_clken rx_rcc   //  Receive DMA e.g. DMA1, DMA_CHANNEL2, NVIC_DMA1_CHANNEL2_IRQ, RCC_DMA1
	) {
	//  Define the pins, DMA and interrupts of the SPI port.  id=1 refers to SPI1.
	if (id < 1 || id > MAX_SPI_PORTS) { return showError(NULL, "spi_init_port", SPI_Invalid_Port); }
	SPI_Control *port = &allPorts[id - 1];
	port->id = id;
	port->tx_semaphore = NULL;
	port->rx_semaphore = NULL;
	port->simulator = NULL;

	//  SPI port config.
	_set(SPIx); _set(ptr_SPI_DR); _set(ptr_SPI_I2SCFGR); _set(RCC_SPIx);			             

	//  GPIO config (port, pin, clock) for each SPI pin (SS, SCK, MISO, MOSI)
	_set(ss_port);   _set(ss_pin);   _set(ss_rcc);
	_set(sck_port);  _set(sck_pin);  _set(sck_rcc);   
	_set(miso_port); _set(miso_pin); _set(miso_rcc);  
	_set(mosi_port); _set(mosi_pin); _set(mosi_rcc);  

	//  DMA config (port, channel, interrupt, clock) for transmit and receive DMA channels.
	_set(tx_dma); _set(tx_channel); _set(tx_irq); _set(tx_rcc);  
	_set(rx_dma); _set(rx_channel); _set(rx_irq); _set(rx_rcc);  

	return SPI_Ok;
}

//  _SPI(1) becomes SPI1, &SPI1_DR, &SPI1_I2SCFGR, RCC_SPI1
#define _SPI(port) \
	SPI ## port, \
	&SPI ## port ## _DR, \
	&SPI ## port ## _I2SCFGR, \
	RCC_SPI ## port

//  _GPIO(A,4) becomes GPIOA, GPIO4, RCC_GPIOA
#define _GPIO(port, pin) \
	GPIO ## port, \
	GPIO ## pin, \
	RCC_GPIO ## port

//  _DMA(1,3) becomes DMA1, DMA_CHANNEL3, NVIC_DMA1_CHANNEL3_IRQ, RCC_DMA1
#define _DMA(port, channel) \
	DMA ## port, \
	DMA_CHANNEL ## channel, \
	NVIC_DMA ## port ## _CHANNEL ## channel ## _IRQ, \
	RCC_DMA ## port

SPI_Control *spi_setup(uint32_t spi_id) {
	//  Enable SPI peripheral and GPIO clocks.  Should be called once only per SPI port.  spi_id=SPI1 or SPI2.
	//  If called the first time, define the pins, DMA and interrupts of each SPI port.
	static bool firstTime = true;
	if (firstTime) {
		firstTime = false;
		//  SPI spec: id, port,   SS,          SCK,         MISO,        MOSI,        TX DMA,    RX DMA
		spi_init_port(1, _SPI(1), _GPIO(A,4),  _GPIO(A,5),  _GPIO(A,6),  _GPIO(A,7),  _DMA(1,3), _DMA(1,2));  //  SPI1
		spi_init_port(2, _SPI(2), _GPIO(B,12), _GPIO(B,13), _GPIO(B,14), _GPIO(B,15), _DMA(1,5), _DMA(1,4));  //  SPI2
	}  //  TODO: Support alternate mapping of SPI pins e.g. PA15, PB3, PB4, PB5.

	//  Find the SPI port.
	uint8_t id = 0;  //  1=SPI1, 2=SPI2.
	for (uint8_t i = 0; i < MAX_SPI_PORTS; i++) {
		if (allPorts[i].SPIx == spi_id) { id = i + 1; break; }
	}
	debug_print("spi setup spi"); debug_println((int) id); debug_flush();
	if (id < 1 || id > MAX_SPI_PORTS) { showError(NULL, "spi_setup", SPI_Invalid_Port); return NULL; }

	//  Fetch the SPI port control.
	SPI_Control *port = &allPorts[id - 1];
	if (port->id != id) { showError(NULL, "spi_setup", SPI_Invalid_Port); return NULL; }

	//  Moved to platform_setup() in bluepill.cpp:
	//  rcc_clock_setup_in_hse_8mhz_out_72mhz();  //  Standard clocks for STM32 Blue Pill.

	//  Enable SPI and DMA clocks so that the SPI and DMA functions will become active.
	rcc_periph_clock_enable((rcc_periph_clken) port->RCC_SPIx);
	rcc_periph_clock_enable((rcc_periph_clken) port->tx_rcc);
	if (port->rx_rcc != port->tx_rcc) { rcc_periph_clock_enable((rcc_periph_clken) port->rx_rcc); }

	//  Enable GPIO clocks so that the GPIO pins used by SPI will become active.
	rcc_periph_clock_enable((rcc_periph_clken) port->ss_rcc);
	if (port->sck_rcc != port->ss_rcc) { rcc_periph_clock_enable((rcc_periph_clken) port->sck_rcc); }
	if (port->miso_rcc != port->ss_rcc && port->miso_rcc != port->sck_rcc) 
		{ rcc_periph_clock_enable((rcc_periph_clken) port->miso_rcc); }
	if (port->mosi_rcc != port->ss_rcc && port->mosi_rcc != port->sck_rcc && port->mosi_rcc != port->miso_rcc) 
		{ rcc_periph_clock_enable((rcc_periph_clken) port->mosi_rcc); }
	return port;
}

//////////////////////////////////////////////////////////////////////////
//  DMA Interrupt Service Routines

static void handle_tx_interrupt(uint32_t spi, uint32_t dma,  uint8_t channel);
static void handle_rx_interrupt(uint32_t spi, uint32_t dma,  uint8_t channel);
static SPI_Control *findPortByDMA(uint32_t dma, uint8_t channel);
static void add_transceive_status(SPI_Control *port, Trans_Status new_status);

//  SPI1 receive interrupt on DMA port 1 channel 2.
void dma1_channel2_isr(void) { handle_rx_interrupt(SPI1, DMA1, DMA_CHANNEL2); }

//  SPI1 transmit interrupt on DMA port 1 channel 3.
void dma1_channel3_isr(void) { handle_tx_interrupt(SPI1, DMA1, DMA_CHANNEL3); }

//  SPI2 receive interrupt on DMA port 1 channel 4.
void dma1_channel4_isr(void) { handle_rx_interrupt(SPI2, DMA1, DMA_CHANNEL4); }

//  SPI2 transmit interrupt on DMA port 1 channel 5.
void dma1_channel5_isr(void) { handle_tx_interrupt(SPI2, DMA1, DMA_CHANNEL5); }

static void handle_tx_interrupt(uint32_t spi, uint32_t dma,  uint8_t channel) {
	//  Handle SPI transmit interrupt on DMA.  Don't call any external functions.
	SPI_Control *port = findPortByDMA(dma, channel);  //  Find the port.

	//  Handle transmit half-done.  Do nothing.
	if (dma_get_interrupt_flag(dma, channel, DMA_HTIF)) { 
		dma_clear_interrupt_flags(dma, channel, DMA_HTIF);
		if (port) { update_transceive_status(port, TRANS_TX_HALFDONE); }
	}
	
	//  Handle transmit complete.
	if (dma_get_interrupt_flag(dma, channel, DMA_TCIF)) {
		dma_clear_interrupt_flags(dma, channel, DMA_TCIF);
		//  Disable interrupts and shutdown DMA.
		disable_interrupts(dma, channel);
		spi_disable_tx_dma(spi);
		dma_disable_channel(dma, channel);
		if (port) {
			if (port->rx_remainder == 0) {  //  This is a normal transceive with tx_len >= rx_len.
				//  Update the transmit complete status.
				update_transceive_status(port);
				//  Signal the transmit semaphore that transmit is done.  (Not used)
				if (port->tx_semaphore) { 
					sem_ISR_signal(*port->tx_semaphore); 
					port->tx_semaphore = NULL;  //  Erase the semaphore so it won't be signalled twice.
				}

			} else {  //  TODO: If tx_len < rx_len, create a dummy transfer to clock in the remaining rx data.
				volatile int rx_remainder = port->rx_remainder;
				port->rx_remainder = 0; // Clear the buffer remainder to skip this section later
				dma_channel_reset(port->tx_dma, port->tx_channel);
				spi_setup_dma(port, port->tx_dma, port->tx_channel, dummy_tx_buf, rx_remainder, false, false);
				enable_interrupts(port->tx_dma, port->tx_channel);
				dma_enable_channel(port->tx_dma, port->tx_channel);
				spi_enable_tx_dma(port->SPIx);
			}
		}
	}

	//  Handle transmit error.
	if (dma_get_interrupt_flag(dma, channel, DMA_TEIF)) { 
		dma_clear_interrupt_flags(dma, channel, DMA_TEIF);
		//  Disable interrupts and shutdown DMA.
		disable_interrupts(dma, channel);
		spi_disable_tx_dma(spi);
		dma_disable_channel(dma, channel);
		if (port) {  //  Update the error status.
			update_transceive_status(port, TRANS_TX_ERROR);
			//  Signal the transmit and receive semaphores that transmit and receive are done.
			if (port->tx_semaphore) { 
				sem_ISR_signal(*port->tx_semaphore); 
				port->tx_semaphore = NULL;  //  Erase the semaphore so it won't be signalled twice.
			}
			if (port->rx_semaphore) { 
				sem_ISR_signal(*port->rx_semaphore); 
				port->rx_semaphore = NULL;  //  Erase the semaphore so it won't be signalled twice.
			}
		}
	}
}

static void handle_rx_interrupt(uint32_t spi, uint32_t dma,  uint8_t channel) {  
	//  Handle SPI receive interrupt on DMA.  Don't call any external functions.
	SPI_Control *port = findPortByDMA(dma, channel);  //  Find the port.

	//  Handle receive half-done.  Do nothing.
	if (dma_get_interrupt_flag(dma, channel, DMA_HTIF)) { 
		dma_clear_interrupt_flags(dma, channel, DMA_HTIF);
		if (port) { update_transceive_status(port, TRANS_RX_HALFDONE); }
	}

	//  Handle receive complete.
	if (dma_get_interrupt_flag(dma, channel, DMA_TCIF)) {
		dma_clear_interrupt_flags(dma, channel, DMA_TCIF);
		//  Disable interrupts and shutdown DMA.
		disable_interrupts(dma, channel);
		spi_disable_rx_dma(spi);
		dma_disable_channel(dma, channel);
		if (port) {  //  Update the receive complete status.
			update_transceive_status(port);
			//  For Event Sensor or Simulator replay: Signal to Sensor Task when receive is done.
			if (port->rx_semaphore) { 
				sem_ISR_signal(*port->rx_semaphore); 
				port->rx_semaphore = NULL;  //  Erase the semaphore so it won't be signalled twice.
			}
		}
	}

	//  Handle receive error.
	if (dma_get_interrupt_flag(dma, channel, DMA_TEIF)) { 
		dma_clear_interrupt_flags(dma, channel, DMA_TEIF);
		//  Disable interrupts and shutdown DMA.
		disable_interrupts(dma, channel);
		spi_disable_rx_dma(spi);
		dma_disable_channel(dma, channel);
		if (port) {  //  Update the error status.
			update_transceive_status(port, TRANS_RX_ERROR); 
			//  For simulator replay: Signal to Sensor Task when receive is done.
			if (port->rx_semaphore) { 
				sem_ISR_signal(*port->rx_semaphore); 
				port->rx_semaphore = NULL;  //  Erase the semaphore so it won't be signalled twice.
			}
		}
	}
}

static void enable_interrupts(uint32_t dma, uint8_t channel) {
	//  Enable DMA transfer complete, error and half-done interrupts for the DMA port and channel.
	dma_enable_transfer_complete_interrupt(dma, channel); 
	dma_enable_transfer_error_interrupt(dma, channel);
	dma_enable_half_transfer_interrupt(dma, channel);
}

static void disable_interrupts(uint32_t dma, uint8_t channel) {
	//  Disable DMA transfer complete, error and half-done interrupts for the DMA port and channel.
	dma_disable_transfer_complete_interrupt(dma, channel); 
	dma_disable_transfer_error_interrupt(dma, channel);
	dma_disable_half_transfer_interrupt(dma, channel);
}

static SPI_Control *findPortByDMA(uint32_t dma, uint8_t channel) {
	//  Called by DMA ISR to return the port by DMA port and channel.  Don't use any external functions.
	for (int i = 0; i < MAX_SPI_PORTS; i++) {
		SPI_Control *port = &allPorts[i];
		if (port->rx_dma == dma && port->rx_channel == channel) { return port; }
		if (port->tx_dma == dma && port->tx_channel == channel) { return port; }
	}
	return NULL;
}

static void update_transceive_status(SPI_Control *port, Trans_Status new_status) {
	//  Called by DMA ISR to update the transceive status.  Don't use any external functions.
	if (port == NULL) { return; }
	if (new_status == (Trans_Status) -1) {
		//  If new status not specified, then transition to next status.
		Trans_Status old_status = port->transceive_status;
		switch(port->transceive_status) {
			case TRANS_NONE: port->transceive_status = TRANS_TX_COMPLETE; break;
			case TRANS_TX_COMPLETE: port->transceive_status = TRANS_RX_COMPLETE; break;
			default: break; //  Nothing.
		}
		//  Record the changed status.
		if (port->transceive_status != old_status) { add_transceive_status(port, port->transceive_status); }
	} else {
		//  Record the new status if error.  Ignore the half done status.
		if (new_status == TRANS_TIMEOUT
			|| new_status == TRANS_TX_ERROR
			|| new_status == TRANS_RX_ERROR) {
			port->transceive_status = new_status;
		}
		add_transceive_status(port, new_status);
	}
}

static void add_transceive_status(SPI_Control *port, Trans_Status new_status) {
	//  Add the transceive status to the history.  Don't use any external functions.
	if (port == NULL) { return; }
	for (int i = 0; i < MAX_TRANS_STATUS; i++) {
		if (port->transceive_history[i] == (Trans_Status) 0) {
			port->transceive_history[i] = new_status;
			i++;
			if (i < MAX_TRANS_STATUS) {
				port->transceive_history[i] = (Trans_Status) 0;
			}
			break;
		}
	}
}

//////////////////////////////////////////////////////////////////////////
//  Legacy Arduino API (Wire SPI Interface)

//  Which SPI port is now sending/receiving. 1=SPI1, 2=SPI2, ...
static uint8_t currentSPIPort = 0;  //  0=Unknown.

//  Allocate one byte per SPI port for send and receive.
static uint8_t tx_buffer[MAX_SPI_PORTS];  //  Must be static because of DMA.
static uint8_t rx_buffer[MAX_SPI_PORTS];

void SPIInterface::beginTransaction(SPIInterfaceSettings settings) {
	//  Used by BME280Spi.cpp
	//  Get the SPI port from the settings.  If not specified, use the last digitalWrite() pin.
	uint8_t portID = settings.port;
	if (portID < 1 || portID > MAX_SPI_PORTS) { portID = currentSPIPort; }
	//  debug_print("spiint begin port "); debug_println((int) portID);  debug_flush();
	if (portID < 1 || portID > MAX_SPI_PORTS) { showError(NULL, "beginTransaction", SPI_Invalid_Port); return; }
	currentSPIPort = portID;
	SPI_Control *port = &allPorts[portID - 1];
	//  SPI setup should have been called in bme280.cpp.  TODO: Verify SPI port number.
	SPI_Fails result = spi_configure(port, settings.speedMaximum, settings.bitOrder, settings.dataMode);
	if (result != SPI_Ok) { return; }
	spi_open(port);
}

uint8_t SPIInterface::transfer(uint8_t data) {
  	//  Send and receive 1 byte of data.  Wait until data is sent and received.  Return the byte received.  Used by BME280Spi.cpp
	//  debug_print("spiint transfer port "); debug_println((int) currentSPIPort);  debug_flush();
	if (currentSPIPort < 1 || currentSPIPort > MAX_SPI_PORTS) { showError(NULL, "transfer", SPI_Invalid_Port); return 0; }
	uint8_t portID = currentSPIPort;
	SPI_Control *port = &allPorts[portID - 1];

	tx_buffer[portID - 1] = data;
	rx_buffer[portID - 1] = 0x22;  //  Means uninitialised.
	SPI_Fails result = spi_transceive_wait(port, &tx_buffer[portID - 1], 1, &rx_buffer[portID - 1], 1);
	if (result != SPI_Ok) { return 0; }
	return rx_buffer[portID - 1];
}

void SPIInterface::endTransaction(void) {
	//  Used by BME280Spi.cpp
	//  debug_print("spiint end port "); debug_println((int) currentSPIPort);  debug_flush();
	if (currentSPIPort < 1 || currentSPIPort > MAX_SPI_PORTS) { showError(NULL, "endTransaction", SPI_Invalid_Port); return; }	
	SPI_Control *port = &allPorts[currentSPIPort - 1];
	spi_close(port);
}

void SPIInterface::pinMode(uint8_t pin, uint8_t mode){
	//  Used by BME280Spi.h
	//  debug_print("pinMode pin "); debug_print((int) pin); debug_print(" mode "); debug_println((int) mode); debug_flush();
}

void SPIInterface::digitalWrite(uint8_t pin, uint8_t val) {
	//  digitalWrite() is called just before an SPI transfer.  We intercept the pin to determine the SPI port.  Used by BME280Spi.h
	const uint32_t port_id = convert_pin_to_port(pin);
	//  Find the SPI port.
	uint8_t id = 0;  //  1=SPI1, 2=SPI2.
	for (uint8_t i = 0; i < MAX_SPI_PORTS; i++) {
		if (allPorts[i].SPIx == port_id) { id = i + 1; break; }
	}
	//  debug_print("digitalWrite pin "); debug_print((int) pin);  debug_print(" id ");  debug_print((int) id);  debug_print(" val "); debug_println((int) val); // debug_flush();
	if (id < 1 || id > MAX_SPI_PORTS) { showError(NULL, "digitalWrite", SPI_Invalid_Port); return; }	
	currentSPIPort = id;
}

SPIInterfaceSettings::SPIInterfaceSettings(uint32_t speedMaximum0, uint8_t bitOrder0, uint8_t dataMode0) {
	//  Used by BME280Spi.cpp
	port = 0;  //  Default to unknown port.
	speedMaximum = speedMaximum0;
	bitOrder = bitOrder0;
	dataMode = dataMode0;
}

//////////////////////////////////////////////////////////////////////////
//  Logging and Error Handling Functions

static void dump_frequency(uint32_t freq);

static void dump_config(SPI_Control *port) {
	//  Dump the SPI port config to console.
	uint32_t freq = get_frequency(port);
	uint32_t baudrate = get_baudrate(port);  	   		 //  e.g. SPI_CR1_BAUDRATE_FPCLK_DIV_256
	uint32_t clock_polarity = get_clock_polarity(port);  //  e.g. SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE
	uint32_t clock_phase = get_clock_phase(port);     	 //  e.g. SPI_CR1_CPHA_CLK_TRANSITION_1
	uint32_t bit_order = get_bit_order(port);			 //  e.g. SPI_CR1_MSBFIRST

	//  Don't dump if same as last dump.
	static uint32_t last_freq = 0;
	static uint32_t last_baudrate = 0;
	static uint32_t last_clock_polarity = 0;
	static uint32_t last_clock_phase = 0;
	static uint32_t last_bit_order = 0;
	if (freq == last_freq && baudrate == last_baudrate && clock_polarity == last_clock_polarity && clock_phase == last_clock_phase
		&& bit_order == last_bit_order) { return; }
	last_freq = freq;
	last_baudrate = baudrate;
	last_clock_polarity = clock_polarity;
	last_clock_phase = clock_phase;
	last_bit_order = bit_order;

	debug_print("  freq "); dump_frequency(port->speedMaximum); debug_print(" -> "); dump_frequency(freq);
	debug_print(" ["); debug_print((int) baudrate); debug_println("]");

	debug_print("  pol "); debug_print((int) port->dataMode); debug_print(" -> "); 
	debug_println(clock_polarity == SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE ? "clk0" 
		: clock_polarity == SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE ? "clk1"
		: "*** UNKNOWN");

	debug_print("  phase "); debug_print((int) port->dataMode); debug_print(" -> "); 
	debug_println(clock_phase == SPI_CR1_CPHA_CLK_TRANSITION_1 ? "trn1"
		: clock_polarity == SPI_CR1_CPHA_CLK_TRANSITION_2 ? "trn2"
		: "*** UNKNOWN");

	debug_print("  order "); debug_print((int) port->bitOrder); debug_print(" -> "); 
	debug_println(bit_order == SPI_CR1_MSBFIRST ? "msb" 
		: bit_order == SPI_CR1_LSBFIRST ? "lsb"
		: "*** UNKNOWN");
}

SPI_Fails spi_dump_trail(SPI_Control *port) {
	//  Dump the simulated commands to console.
	if (port->simulator == NULL) { return showError(NULL, "spi_dump_trail", SPI_Missing_Simulator); }  //  No simulator.
	const char *title = get_mode_name(port);
	int i = port->simulator->index;
	port->simulator->index = 0;
	debug_print(title); debug_print(" ");
	for (;;) {
		//  Read the captured SPI packet for send and receive.
		int tx_len = simulator_replay_size(port->simulator);
		uint8_t *tx_buf = simulator_replay_packet(port->simulator, tx_len);
		int rx_len = simulator_replay_size(port->simulator);
		uint8_t *rx_buf = simulator_replay_packet(port->simulator, rx_len);
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

SPI_Fails spi_dump_packet(SPI_Control *port) {
	//  Dump the last SPI packet to console.
	const char *title = get_mode_name(port);
	dump_packets(title, port->tx_buf, port->tx_len, port->rx_buf, port->rx_len);
	return SPI_Ok;
}

static void dump_packets(const char *title, SPI_DATA_TYPE *tx_buf, int tx_len, SPI_DATA_TYPE *rx_buf, int rx_len) {
	//  Print the contents of the packets.
	debug_print(title); debug_print(" >> ");
	for (int i = 0; i < tx_len; i++) { debug_printhex(tx_buf[i]); debug_print(" "); }
	debug_print("<< ");
	for (int i = 0; i < rx_len; i++) { debug_printhex(rx_buf[i]); debug_print(" "); } 
	debug_println(""); // debug_flush();
}

static void dump_packet(const char *title, SPI_DATA_TYPE *buf, int len) {
	//  Print the contents of the packet.
	debug_print(title); debug_print(" ");
	for (int i = 0; i < len; i++) { debug_printhex(buf[i]); debug_print(" "); } 
}

static void dump_frequency(uint32_t freq) {
	//  Dump the frequency to console e.g. 1.12M, 562.5k.
	if (freq >= 1000000) { debug_print((float) (freq / 1000000.0)); debug_print("M"); }
	else if (freq >= 1000) { debug_print((float) (freq / 1000.0)); debug_print("k"); }
	else { debug_print((int) freq); }
}

static void dump_history(SPI_Control *port) {
	//  Dump the history of status transitions.
	debug_print("hist ");
	for (int i = 0; i < MAX_TRANS_STATUS; i++) {
		volatile Trans_Status status = port->transceive_history[i];
		if (status == (Trans_Status) 0) { break; }
		debug_print((int) status); debug_print(" ");
	}
	debug_println("");
}

//  Messages for each fail code.
static const char *spi_msg[] = {
	"OK",
	"Missing Simulator",  //  SPI_Missing_Simulator
	"Invalid Port",  //  SPI_Invalid_Port
	"Invalid Size",  //  SPI_Invalid_Size
	"Simulation Mismatch",  //  SPI_Mismatch
	"Timeout"  //  SPI_Timeout
};

static inline TickType_t diff_ticks(TickType_t early, TickType_t later) {
    //  Compute the time difference in ticks (milliseconds).
	if (later >= early) { return later - early; }
	return ~(TickType_t)0 - early + 1 + later;
}

const char *spi_error(SPI_Fails fcode) {
    //  Return a character string message for SPI_Fails code
	int icode = (int)fcode;
	if ( icode < 0 || icode >= (int)Simulator_End )
		return "Bad SPI_Fails code";
	return spi_msg[icode];
}

static SPI_Fails showError(SPI_Control *port, const char *title, SPI_Fails fc) {
	//  Show the message for the error code on console.
	if (port) { port->failCode = fc; }
	debug_print("***** Error: SPI Failed ");
	debug_print(title); debug_print(" ");
	debug_println(spi_error(fc));
	debug_flush();
	return fc;
}

static const char *get_mode_name(SPI_Control *port) {
	//  Return the mode name.
	const char *title = "";
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
