//  Stubs to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
//  Used by BME280 library.
#ifndef WIRE_H_
#define WIRE_H_

#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t

//  From Arduino Wire.h
class TwoWire  //  : public Stream
{
  public:
    TwoWire() {}  //  TODO
    void begin() {}  //  TODO: Used by bme280.cpp
    // void begin(uint8_t);
    // void begin(int);
    // void end();
    // void setClock(uint32_t);
    void beginTransmission(uint8_t);  //  TODO: Used by BME280I2C.cpp
    // void beginTransmission(int);
    uint8_t endTransmission(void);  //  TODO: Used by BME280I2C.cpp
    // uint8_t endTransmission(uint8_t);
    uint8_t requestFrom(uint8_t addr, uint8_t length);  //  TODO: Used by BME280I2C.cpp
    // uint8_t requestFrom(uint8_t, uint8_t, uint8_t);
	// uint8_t requestFrom(uint8_t, uint8_t, uint32_t, uint8_t, uint8_t);
    // uint8_t requestFrom(int, int);
    // uint8_t requestFrom(int, int, int);
    virtual size_t write(uint8_t);  //  TODO: Used by BME280I2C.cpp
    // virtual size_t write(const uint8_t *, size_t);
    virtual int available(void);  //  TODO: Used by BME280I2C.cpp
    virtual int read(void);  //  TODO: Used by BME280I2C.cpp
    // virtual int peek(void);
    // virtual void flush(void);
    // void onReceive( void (*)(int) );
    // void onRequest( void (*)(void) );

    // inline size_t write(unsigned long n) { return write((uint8_t)n); }
    // inline size_t write(long n) { return write((uint8_t)n); }
    // inline size_t write(unsigned int n) { return write((uint8_t)n); }
    // inline size_t write(int n) { return write((uint8_t)n); }
    // using Print::write;
};

extern TwoWire Wire;

#endif  //  WIRE_H_
