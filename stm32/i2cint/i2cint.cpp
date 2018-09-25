//  I2C Interface for STM32 Blue Pill. Compatible with Arduino's Wire I2C interface.  Based on:
//  https://github.com/Apress/Beg-STM32-Devel-FreeRTOS-libopencm3-GCC/tree/master/rtos/i2c-pcf8574

#include "../../platform.h"  //  For SIMULATE_BME280
#include <logger.h>
#include "i2cint.h"

#ifndef SIMULATE_BME280  //  Implement a real I2C interface.
#include <stdbool.h>
#include <libopencm3/stm32/i2c.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/timer.h>
#include <bluepill.h>  //  For millis()

//  TODO: Update this when running under FreeRTOS.
#define systicks millis
typedef uint32_t TickType_t;
static void taskYIELD(void) {}
static void taskENTER_CRITICAL(void) {}
static void taskEXIT_CRITICAL(void) {}

//  Messages for each fail code.
static const char *i2c_msg[] = {
	"OK",
	"Address timeout",
	"Address NAK",
	"Write timeout",
	"Read timeout"
};

/*********************************************************************
 * Compute the difference in ticks:
 *********************************************************************/

static inline TickType_t
diff_ticks(TickType_t early,TickType_t later) {
	if ( later >= early )
		return later - early;
	return ~(TickType_t)0 - early + 1 + later;
}

/*********************************************************************
 * Return a character string message for I2C_Fails code
 *********************************************************************/

const char *i2c_error(I2C_Fails fcode) {
	int icode = (int)fcode;
	if ( icode < 0 || icode > (int)I2C_Read_Timeout )
		return "Bad I2C_Fails code";
	return i2c_msg[icode];
}

static I2C_Fails showError(I2C_Control *dev, I2C_Fails fc) {
	if (dev) { dev->failCode = fc; }
	debug_print("***** Error: I2C Failed ");
	debug_print(fc); debug_print(" / ");
	debug_println(i2c_error(fc));
	debug_flush();
	return fc;
}

/*********************************************************************
 * Peripheral setup:
 *********************************************************************/

I2C_Fails i2c_setup(void) {
	//  Init the I2C clock and I2C pin levels.  Should be called once only.
	
	//  Moved to platform_setup() in bluepill.cpp:
	//  rcc_clock_setup_in_hse_8mhz_out_72mhz();
	rcc_periph_clock_enable(RCC_GPIOB);	// I2C
	rcc_periph_clock_enable(RCC_AFIO);	// EXTI
	rcc_periph_clock_enable(RCC_I2C1);	// I2C

	gpio_set_mode(GPIOB,
		GPIO_MODE_OUTPUT_50_MHZ,
		GPIO_CNF_OUTPUT_ALTFN_OPENDRAIN,
		GPIO6|GPIO7);					// I2C
	gpio_set(GPIOB,GPIO6|GPIO7);		// Idle high

	// AFIO_MAPR_I2C1_REMAP=0, PB6+PB7
	gpio_primary_remap(0,0);
	return I2C_Ok;
}

/*********************************************************************
 * Configure I2C device for 100 kHz, 7-bit addresses
 *********************************************************************/

I2C_Fails i2c_configure(I2C_Control *dev, uint32_t i2c, uint32_t ticks) {
	dev->device = i2c;
	dev->addr = 0;
	dev->reg = 0;
	dev->isRegSet = false;
	dev->length = 0;
	dev->timeout = ticks;
	dev->failCode = I2C_Ok;
	i2c_peripheral_disable(dev->device);
	i2c_reset(dev->device);
	I2C_CR1(dev->device) &= ~I2C_CR1_STOP;	// Clear stop
	i2c_set_standard_mode(dev->device);	// 100 kHz mode
	i2c_set_clock_frequency(dev->device,I2C_CR2_FREQ_36MHZ); // APB Freq
	i2c_set_trise(dev->device,36);		// 1000 ns
	i2c_set_dutycycle(dev->device,I2C_CCR_DUTY_DIV2);
	i2c_set_ccr(dev->device,180);		// 100 kHz <= 180 * 1 /36M
	i2c_set_own_7bit_slave_address(dev->device,0x23); // Necessary?
	i2c_peripheral_enable(dev->device);
	return I2C_Ok;
}

/*********************************************************************
 * Return when I2C is not busy
 *********************************************************************/

I2C_Fails i2c_wait_busy(I2C_Control *dev) {
	while ( I2C_SR2(dev->device) & I2C_SR2_BUSY )
		taskYIELD();			// I2C Busy
	return I2C_Ok;
}

/*********************************************************************
 * Start I2C Read/Write Transaction with indicated 7-bit address:
 *********************************************************************/

I2C_Fails i2c_start_addr(I2C_Control *dev, uint8_t addr, enum I2C_RW rw) {
	TickType_t t0 = systicks();
	i2c_wait_busy(dev);					 // Block until not busy
	I2C_SR1(dev->device) &= ~I2C_SR1_AF; // Clear Acknowledge failure
	i2c_clear_stop(dev->device);		 // Do not generate a Stop
	if ( rw == Read )
		i2c_enable_ack(dev->device);
	i2c_send_start(dev->device);		 // Generate a Start/Restart

	// Loop until ready:
    while ( !((I2C_SR1(dev->device) & I2C_SR1_SB) 
	  && (I2C_SR2(dev->device) & (I2C_SR2_MSL|I2C_SR2_BUSY))) ) {
		if ( diff_ticks(t0,systicks()) > dev->timeout )
			return showError(dev, I2C_Addr_Timeout);
		taskYIELD();
	}

	// Send Address & R/W flag:
	i2c_send_7bit_address(dev->device, addr,
		rw == Read ? I2C_READ : I2C_WRITE);

	// Wait until completion, NAK or timeout
	t0 = systicks();
	while ( !(I2C_SR1(dev->device) & I2C_SR1_ADDR) ) {
		if ( I2C_SR1(dev->device) & I2C_SR1_AF ) {
			i2c_send_stop(dev->device);
			(void)I2C_SR1(dev->device);
			(void)I2C_SR2(dev->device); 	// Clear flags
			// NAK Received (no ADDR flag will be set here)
			return showError(dev, I2C_Addr_NAK); 
		}
		if ( diff_ticks(t0,systicks()) > dev->timeout )
			return showError(dev, I2C_Addr_Timeout); 
		taskYIELD();
	}

	(void)I2C_SR2(dev->device);		// Clear flags
	return I2C_Ok;
}

/*********************************************************************
 * Write one byte of data
 *********************************************************************/

I2C_Fails i2c_write(I2C_Control *dev,uint8_t byte) {
	TickType_t t0 = systicks();

	i2c_send_data(dev->device,byte);
	while ( !(I2C_SR1(dev->device) & (I2C_SR1_BTF)) ) {
		if ( diff_ticks(t0,systicks()) > dev->timeout )
			return showError(dev, I2C_Write_Timeout);
		taskYIELD();
	}
	return I2C_Ok;
}

/*********************************************************************
 * Read one byte of data. Set lastf=true, if this is the last/only
 * byte being read. Return -1 in case of error.
 *********************************************************************/

int i2c_read(I2C_Control *dev,bool lastf) {
	TickType_t t0 = systicks();

	if ( lastf )
		i2c_disable_ack(dev->device);	// Reading last/only byte

	while ( !(I2C_SR1(dev->device) & I2C_SR1_RxNE) ) {
		if ( diff_ticks(t0,systicks()) > dev->timeout )
			{ showError(dev, I2C_Read_Timeout); return -1; }
		taskYIELD();
	}
	
	return i2c_get_data(dev->device);
}

/*********************************************************************
 * Write one byte of data, then initiate a repeated start for a
 * read to follow.
 *********************************************************************/

I2C_Fails i2c_write_restart(I2C_Control *dev,uint8_t byte,uint8_t addr) {
	TickType_t t0 = systicks();

	taskENTER_CRITICAL();
	i2c_send_data(dev->device,byte);
	// Must set start before byte has written out
	i2c_send_start(dev->device);
	taskEXIT_CRITICAL();

	// Wait for transmit to complete
	while ( !(I2C_SR1(dev->device) & (I2C_SR1_BTF)) ) {
		if ( diff_ticks(t0,systicks()) > dev->timeout )
			return showError(dev, I2C_Write_Timeout);
		taskYIELD();
	}

	// Loop until restart ready:
	t0 = systicks();
    while ( !((I2C_SR1(dev->device) & I2C_SR1_SB) 
	  && (I2C_SR2(dev->device) & (I2C_SR2_MSL|I2C_SR2_BUSY))) ) {
		if ( diff_ticks(t0,systicks()) > dev->timeout )
			return showError(dev, I2C_Addr_Timeout);
		taskYIELD();
	}

	// Send Address & Read command bit
	i2c_send_7bit_address(dev->device,addr,I2C_READ);

	// Wait until completion, NAK or timeout
	t0 = systicks();
	while ( !(I2C_SR1(dev->device) & I2C_SR1_ADDR) ) {
		if ( I2C_SR1(dev->device) & I2C_SR1_AF ) {
			i2c_send_stop(dev->device);
			(void)I2C_SR1(dev->device);
			(void)I2C_SR2(dev->device); 	// Clear flags
			// NAK Received (no ADDR flag will be set here)
			return showError(dev, I2C_Addr_NAK); 
		}
		if ( diff_ticks(t0,systicks()) > dev->timeout )
			return showError(dev, I2C_Addr_Timeout); 
		taskYIELD();
	}

	(void)I2C_SR2(dev->device);		// Clear flags
	return I2C_Ok;
}

I2C_Fails i2c_stop(I2C_Control *dev) {
	//  Stop the I2C connection.
	i2c_send_stop(dev->device);
	return I2C_Ok;
}

#ifdef NOTUSED  //  Calling convention used by BME280 library.
bool BME280I2C::WriteRegister(uint8_t addr, uint8_t data) {
  Wire.beginTransmission(m_bme_280_addr);
  Wire.write(addr);
  Wire.write(data);
  Wire.endTransmission();
  return true;
}

bool BME280I2C::ReadRegister(uint8_t addr, uint8_t data[], uint8_t length) {
  uint8_t ord(0);
  Wire.beginTransmission(m_bme_280_addr);
  Wire.write(addr);
  Wire.endTransmission();
  Wire.requestFrom(m_bme_280_addr, length);
  while(Wire.available()) { data[ord++] = Wire.read(); }
  return ord == length;
}
#endif  //  NOTUSED

static I2C_Control i2c;			// I2C Control struct

I2CInterface::I2CInterface() {
    //  Init the I2C interface.  This constructor is called before platform_setup(),
    //  so we don't do any setup yet till later.
}

void I2CInterface::begin() {  //  Used by bme280.cpp
	//  Set up the timers and configure the I2C port.
	//  debug_println("i2c_begin"); debug_flush();
	i2c_setup();
	i2c_configure(&i2c, I2C1, 1000);
}

void I2CInterface::beginTransmission(uint8_t addr) {  //  Used by BME280I2C.cpp
	//  Begin an I2C request to write register ID or register ID + data value.
	//  debug_print("i2c_begintrans "); debug_println((int) addr); debug_flush();
	i2c.addr = addr;
	i2c.reg = 0;
	i2c.isRegSet = false;
	i2c.length = 0;
	i2c.failCode = I2C_Ok;
	i2c_start_addr(&i2c, addr, Write);
}

size_t I2CInterface::write(uint8_t registerIDOrValue) {  //  Used by BME280I2C.cpp
    //  Write a register ID or data value to the I2C Bus.  Arduino's Wire I2C interface
	//  sends the register ID to the first write(), data value to the second write().
	//  Wherever possible, use the two-parameter version of write() below.
	//  Return the number of bytes written.
    //  debug_print("i2c_write: "); debug_println((int) registerIDOrValue);
	if (i2c.failCode > 0) {
		//  Skip the write if we had an error earlier.
    	debug_println("i2c_write skipped due to earlier error"); debug_flush();
		return 0;
	}
	if (!i2c.isRegSet) {
		//  First write must be register ID.
		i2c.reg = registerIDOrValue;
		i2c.isRegSet = true;
	} else {
		//  Second write must be value.
	}
	I2C_Fails fc = i2c_write(&i2c, registerIDOrValue);
	if (fc > 0) return 0;
	return 1;
}

size_t I2CInterface::write(uint8_t registerID, uint8_t value) {
    //  Write a register ID then data value to the I2C Bus.
	//  Return the number of bytes written.
    //  debug_print("i2c_write reg: "); debug_println((int) registerID);
	if (i2c.failCode > 0) {
		//  Skip the write if we had an error earlier.
    	debug_println("i2c_write skipped due to earlier error"); debug_flush();
		return 0;
	}
	i2c.reg = registerID;
	i2c.isRegSet = true;
	I2C_Fails fc = i2c_write(&i2c, registerID);
	if (fc > 0) { return 0; }
	fc = i2c_write(&i2c, value);
	if (fc > 0) { return 1; }
	return 2;
}

uint8_t I2CInterface::endTransmission(void) {  //  Used by BME280I2C.cpp
	//  End the write request to the I2C Bus.
    //  debug_println("i2c_endtrans");
	if (i2c.failCode > 0) {
		//  Skip the request if we had an error earlier.
    	debug_println("i2c_endtrans skipped due to earlier error"); debug_flush();
		return 0;
	}
	I2C_Fails fc = i2c_stop(&i2c);
	if (fc > 0) { return 0; }
	return 1;
}

uint8_t I2CInterface::requestFrom(uint8_t addr, uint8_t length) {  //  Used by BME280I2C.cpp
    //  Begin an I2C request to read "length" number of bytes from the address at "addr".
    //  debug_print("i2c_request addr: "); debug_print((int) addr); debug_print(", reg: "); debug_print((int) i2c.reg); debug_print(", length: "); debug_println(length);
	if (i2c.failCode > 0) {
		//  Skip the read if we had an error earlier.
		i2c.addr = addr;
		i2c.length = 0;
    	debug_println("i2c_request skipped due to earlier error"); debug_flush();
		return 0;
	}
	i2c.addr = addr;
	i2c.length = length;
	I2C_Fails fc = i2c_start_addr(&i2c, addr, Read);
	if (fc > 0) { 
		i2c.length = 0;
		return 0; 
	}
    return length;
}

int I2CInterface::available(void) {  //  Used by BME280I2C.cpp
    //  Return the number of bytes to be read from the I2C address.
    return i2c.length;
}

int I2CInterface::read(void) {  //  Used by BME280I2C.cpp
    //  Return the next byte to be read from the I2C address.
    //  debug_println("i2c_read");
	if (i2c.length <= 0) { return -1; }
	i2c.length--;	

	bool lastByte = (i2c.length == 0);
	int b = i2c_read(&i2c, lastByte);
	if (b < 0) return b;
	if (lastByte) { 
		i2c_stop(&i2c); 
		//  debug_println((int) b); debug_flush();
	} else {
		//  debug_print((int) b); debug_print(" ");
	}
    return b;
}
#endif  //  !SIMULATE_BME280

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
#endif  // SIMULATE_BME280
