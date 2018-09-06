//  I2C Interface for STM32 Blue Pill. Compatible with Arduino's Wire I2C interface.  Based on:
//  https://github.com/libopencm3/libopencm3-examples/blob/master/examples/stm32/f1/other/i2c_stts75_sensor/i2c_stts75_sensor.c
//  https://github.com/libopencm3/libopencm3-examples/blob/master/examples/stm32/f1/other/i2c_stts75_sensor/stts75.c

//  #define SIMULATE_BME280  //  Uncomment to simulate a BME280 sensor connected to I2C Bus.
#include <logger.h>
#include "i2cint.h"

#ifndef SIMULATE_BME280  //  Implement a real I2C interface.
//  We support only Blue Pill I2C Port 2:
//  SDA2 = Pin PB11
//  SCL2 = Pin PB10
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/flash.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/usart.h>
#include <libopencm3/stm32/i2c.h>

static void i2c_setup(void) {
    /* Enable GPIOB clock. */
	rcc_periph_clock_enable(RCC_GPIOB);  //  TODO

	/* Enable clocks for I2C2 and AFIO. */
	rcc_periph_clock_enable(RCC_I2C2);
	rcc_periph_clock_enable(RCC_AFIO);

	/* Set alternate functions for the SCL and SDA pins of I2C2. */
	gpio_set_mode(GPIOB, GPIO_MODE_OUTPUT_50_MHZ,
		      GPIO_CNF_OUTPUT_ALTFN_OPENDRAIN,
		      GPIO_I2C2_SCL | GPIO_I2C2_SDA);

	/* Disable the I2C before changing any configuration. */
	i2c_peripheral_disable(I2C2);

	/* APB1 is running at 36MHz. */
	i2c_set_clock_frequency(I2C2, I2C_CR2_FREQ_36MHZ);

	/* 400KHz - I2C Fast Mode */
	i2c_set_fast_mode(I2C2);

	/*
	 * fclock for I2C is 36MHz APB2 -> cycle time 28ns, low time at 400kHz
	 * incl trise -> Thigh = 1600ns; CCR = tlow/tcycle = 0x1C,9;
	 * Datasheet suggests 0x1e.
	 */
	i2c_set_ccr(I2C2, 0x1e);

	/*
	 * fclock for I2C is 36MHz -> cycle time 28ns, rise time for
	 * 400kHz => 300ns and 100kHz => 1000ns; 300ns/28ns = 10;
	 * Incremented by 1 -> 11.
	 */
	i2c_set_trise(I2C2, 0x0b);

	/*
	 * This is our slave address - needed only if we want to receive from
	 * other masters.
	 */
	i2c_set_own_7bit_slave_address(I2C2, 0x32);

	/* If everything is configured -> enable the peripheral. */
	i2c_peripheral_enable(I2C2);
}

#ifdef NOTUSED
uint16_t stts75_read_temperature(uint32_t i2c, uint8_t sensor)
{
	uint32_t reg32 __attribute__((unused));
	uint16_t temperature;

	// Wire.beginTransmission(m_bme_280_addr);
	
	/* Send START condition. */
	i2c_send_start(i2c);

	/* Waiting for START is send and switched to master mode. */
	while (!((I2C_SR1(i2c) & I2C_SR1_SB)
		& (I2C_SR2(i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))));

	// Wire.write(addr);
	
	/* Say to what address we want to talk to. */
	/* Yes, WRITE is correct - for selecting register in STTS75. */
	i2c_send_7bit_address(i2c, sensor, I2C_WRITE);

	/* Waiting for address is transferred. */
	while (!(I2C_SR1(i2c) & I2C_SR1_ADDR));

	// Wire.endTransmission();

	// Wire.requestFrom(m_bme_280_addr, length);
	// while(Wire.available()) { data[ord++] = Wire.read(); }
	// return ord == length;

	/* Cleaning ADDR condition sequence. */
	reg32 = I2C_SR2(i2c);

	i2c_send_data(i2c, 0x0); /* temperature register */
	while (!(I2C_SR1(i2c) & (I2C_SR1_BTF | I2C_SR1_TxE)));

	/*
	 * Now we transferred that we want to ACCESS the temperature register.
	 * Now we send another START condition (repeated START) and then
	 * transfer the destination but with flag READ.
	 */

	/* Send START condition. */
	i2c_send_start(i2c);

	/* Waiting for START is send and switched to master mode. */
	while (!((I2C_SR1(i2c) & I2C_SR1_SB)
		& (I2C_SR2(i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))));

	/* Say to what address we want to talk to. */
	i2c_send_7bit_address(i2c, sensor, I2C_READ); 

	/* 2-byte receive is a special case. See datasheet POS bit. */
	I2C_CR1(i2c) |= (I2C_CR1_POS | I2C_CR1_ACK);

	/* Waiting for address is transferred. */
	while (!(I2C_SR1(i2c) & I2C_SR1_ADDR));

	/* Cleaning ADDR condition sequence. */
	reg32 = I2C_SR2(i2c);

	/* Cleaning I2C_SR1_ACK. */
	I2C_CR1(i2c) &= ~I2C_CR1_ACK;

	/* Now the slave should begin to send us the first byte. Await BTF. */
	while (!(I2C_SR1(i2c) & I2C_SR1_BTF));
	temperature = (uint16_t)(I2C_DR(i2c) << 8); /* MSB */

	/*
	 * Yes they mean it: we have to generate the STOP condition before
	 * saving the 1st byte.
	 */
	I2C_CR1(i2c) |= I2C_CR1_STOP;

	temperature |= I2C_DR(i2c); /* LSB */

	/* Original state. */
	I2C_CR1(i2c) &= ~I2C_CR1_POS;

	return temperature;
}

bool BME280I2C::WriteRegister
(
  uint8_t addr,
  uint8_t data
)
{
  Wire.beginTransmission(m_bme_280_addr);
  Wire.write(addr);
  Wire.write(data);
  Wire.endTransmission();
  return true; // TODO: Chech return values from wire calls.
}

bool BME280I2C::ReadRegister
(
  uint8_t addr,
  uint8_t data[],
  uint8_t length
)
{
  uint8_t ord(0);
  Wire.beginTransmission(m_bme_280_addr);
  Wire.write(addr);
  Wire.endTransmission();
  Wire.requestFrom(m_bme_280_addr, length);
  while(Wire.available()) { data[ord++] = Wire.read(); }
  return ord == length;
}

int OLDmain(void) {
	int i = 0;
	uint16_t temperature;

	rcc_clock_setup_in_hse_16mhz_out_72mhz();
	gpio_setup();
	usart_setup();
	i2c_setup();

	gpio_clear(GPIOB, GPIO7);	/* LED1 on */
	gpio_set(GPIOB, GPIO6);		/* LED2 off */

	/* Send a message on USART1. */
	usart_send(USART1, 's');
	usart_send(USART1, 't');
	usart_send(USART1, 'm');
	usart_send(USART1, '\r');
	usart_send(USART1, '\n');

	stts75_write_config(I2C2, STTS75_SENSOR0);
	stts75_write_temp_os(I2C2, STTS75_SENSOR0, 0x1a00); /* 26 degrees */
	stts75_write_temp_hyst(I2C2, STTS75_SENSOR0, 0x1a00);
	temperature = stts75_read_temperature(I2C2, STTS75_SENSOR0);

	/* Send the temperature as binary over USART1. */
	for (i = 15; i >= 0; i--) {
		if (temperature & (1 << i))
			usart_send(USART1, '1');
		else
			usart_send(USART1, '0');
	}

	usart_send(USART1, '\r');
	usart_send(USART1, '\n');

	gpio_clear(GPIOB, GPIO6); /* LED2 on */

	while (1); /* Halt. */

	return 0;
}
#endif  //  NOTUSED

static uint32_t i2cAddr = 0;

I2CInterface::I2CInterface() {
    //  Init the I2C interface.  This constructor is called before platform_setup(),
    //  so we don't do any setup yet till later.
}

void I2CInterface::begin() {  //  Used by bme280.cpp
	i2c_setup();
}

void I2CInterface::beginTransmission(uint8_t i2c) {  //  Used by BME280I2C.cpp
	i2cAddr = i2c;

	/* Send START condition. */
	i2c_send_start(i2c);

	/* Waiting for START is send and switched to master mode. */
	while (!((I2C_SR1(i2c) & I2C_SR1_SB)
		& (I2C_SR2(i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))));

	/* Cleaning ADDR condition sequence. */
	uint32_t reg32 __attribute__((unused));	
	reg32 = I2C_SR2(i2c);
}

uint8_t I2CInterface::endTransmission(void) {  //  Used by BME280I2C.cpp
	/* Send STOP condition. */
	uint32_t i2c = i2cAddr;
	i2c_send_stop(i2c);
	return 1;
}

size_t I2CInterface::write(uint8_t sensor) {  //  Used by BME280I2C.cpp
    //  Simulate the handling of a request to read a register at "dataRegister0".
    //  debug_print("i2c_write reg: "); debug_println(dataRegister0);
	uint32_t i2c = i2cAddr;

	/* Say to what address we want to talk to. */
	// i2c_send_7bit_address(i2c, sensor, I2C_WRITE);
	// i2c_send_7bit_address(i2c, sensor, I2C_READ);

	/* Waiting for address is transferred. */
	// while (!(I2C_SR1(i2c) & I2C_SR1_ADDR));
	
	//  Send data.
	i2c_send_data(i2c, sensor);
	while (!(I2C_SR1(i2c) & (I2C_SR1_BTF | I2C_SR1_TxE)));

    return 1;
}

static uint8_t i2cRequestLength = 0;

uint8_t I2CInterface::requestFrom(uint8_t i2c, uint8_t length) {  //  Used by BME280I2C.cpp
    //  Simulate the handling of a request to read "length" number of bytes from the register at "dataRegister".
    //  debug_print("i2c_request reg: "); debug_println(dataRegister);
	i2cAddr = i2c;
	i2cRequestLength = length;

	/* Cleaning ADDR condition sequence. */
	uint32_t reg32 __attribute__((unused));
	reg32 = I2C_SR2(i2c);

	/* Cleaning I2C_SR1_ACK. */
	I2C_CR1(i2c) &= ~I2C_CR1_ACK;

	/* Now the slave should begin to send us the first byte. Await BTF. */
	while (!(I2C_SR1(i2c) & I2C_SR1_BTF));

    return 0;
}

int I2CInterface::available(void) {  //  Used by BME280I2C.cpp
    //  Return the number of simulated bytes to be read from the I2C address.
    return i2cRequestLength;
}

int I2CInterface::read(void) {  //  Used by BME280I2C.cpp
    //  Return the next simulated byte to be read from the I2C address.
    //  debug_println("i2c_read");
	uint32_t i2c = i2cAddr;
	if (i2cRequestLength <= 0) { return -1; }

	/*
	 * Yes they mean it: we have to generate the STOP condition before
	 * saving the 1st byte.
	 */
	if (i2cRequestLength == 0) I2C_CR1(i2c) |= I2C_CR1_STOP;
	int b = I2C_DR(i2c);

	/* Original state. */
	if (i2cRequestLength == 0) I2C_CR1(i2c) &= ~I2C_CR1_POS;
	i2cRequestLength--;
	
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
