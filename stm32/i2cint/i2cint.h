//  I2C Interface for STM32. Compatible with Arduino's Wire I2C interface.
//  We support only Blue Pill I2C Port 1:
//  SCL1 = Pin PB6
//  SDA1 = Pin PB7
#ifndef I2CINT_H_
#define I2CINT_H_
#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  This is the new I2C Interface.  New code should use this.
/* A stm32f103 application I2C library
 * Warren W. Gay VE3WWG
 * Sat Nov 25 11:56:51 2017
 *
 * Notes:
 *	1. Master I2C mode only
 *	2. No interrupts are used
 *	3. ReSTART I2C is not supported
 *	4. Uses PB6=SCL, PB7=SDA
 *	5. Requires GPIOB clock enabled
 *	6. PB6+PB7 must be GPIO_CNF_OUTPUT_ALTFN_OPENDRAIN
 *	7. Requires rcc_periph_clock_enable(RCC_I2C1);
 *	8. Requires rcc_periph_clock_enable(RCC_AFIO);
 *	9. 100 kHz
 */

/*********************************************
 * This example performs a write transaction,
 * followed by a separate read transaction:
 *********************************************
i2c_start_addr(&i2c, addr, Write);
i2c_write(&i2c, value & 0x0FF);
i2c_stop(&i2c);

i2c_start_addr(&i2c, addr, Read);
byte = i2c_read(&i2c, true);
i2c_stop(&i2c);
*/

/*********************************************
 * This example performs a write followed
 * immediately by a read in one I2C transaction,
 * using a "Repeated Start"
 *********************************************
i2c_start_addr(&i2c, addr, Write);
i2c_write_restart(&i2c, value&0x0FF, addr);
byte = i2c_read(&i2c, true);
i2c_stop(&i2c);
*/

typedef enum {
	I2C_Ok = 0,
	I2C_Addr_Timeout,
	I2C_Addr_NAK,
	I2C_Write_Timeout,
	I2C_Read_Timeout
} I2C_Fails;

enum I2C_RW {
	Read = 1,
	Write = 0
};

typedef struct {
	uint32_t	device;		// STM32 I2C device
	uint8_t		addr;		// I2C address to be read/written
	uint8_t		reg;		// I2C register to be read/written
	bool		isRegSet;	// True if reg has been set
	uint8_t		length;		// Requested length	
	uint32_t	timeout;	// Ticks (millisecond)
	I2C_Fails	failCode;   // Last fail code.
} I2C_Control;

I2C_Fails i2c_setup(void);  	//  Init the I2C clock and I2C pin levels.  Should be called once only.
I2C_Fails i2c_configure(I2C_Control *dev, uint32_t i2c, uint32_t ticks);  //  Configure I2C device for 100 kHz, 7-bit addresses.
I2C_Fails i2c_wait_busy(I2C_Control *dev);  //  Return when I2C is not busy.
I2C_Fails i2c_start_addr(I2C_Control *dev, uint8_t addr, enum I2C_RW rw);  //  Start I2C Read/Write Transaction with indicated 7-bit address.
I2C_Fails i2c_write(I2C_Control *dev,uint8_t byte);  //  Write one byte of data.
I2C_Fails i2c_write_restart(I2C_Control *dev, uint8_t byte, uint8_t addr);  //  Write one byte of data, then initiate a repeated start for a read to follow.
int i2c_read(I2C_Control *dev, bool lastf);  //  Read one byte of data. Set lastf=true, if this is the last/only byte being read.  Return -1 in case of error.
I2C_Fails i2c_stop(I2C_Control *dev);  //  Close the I2C request.

//  For error handling.
const char *i2c_error(I2C_Fails fcode);  //  Get error message for I2C_Fails code.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#ifdef __cplusplus  //  I2CInterface class for C++ only
//  This is the legacy I2C Interface for Arduino.  New code should NOT use this.
class I2CInterface {
  public:
    I2CInterface();
    void begin();  //  Used by bme280.cpp
    void beginTransmission(uint8_t);  //  Used by BME280I2C.cpp
    size_t write(uint8_t registerIDOrValue);  //  Used by BME280I2C.cpp
    size_t write(uint8_t registerID, uint8_t value);  //  New code should use this.
    uint8_t endTransmission(void);  //  Used by BME280I2C.cpp
    uint8_t requestFrom(uint8_t addr, uint8_t length);  //  Used by BME280I2C.cpp
    int available(void);  //  Used by BME280I2C.cpp
    int read(void);  //  Used by BME280I2C.cpp
};
#endif  //  __cplusplus
#endif  //  I2CINT_H_
