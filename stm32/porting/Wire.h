//  Declarations to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
//  Used by BME280 library.
#ifndef WIRE_H_
#define WIRE_H_

#include <stdint.h>

//  From Arduino Wire.h
class TwoWire  //  : public Stream
{
  public:
    TwoWire() {}  //  TODO
    void begin() {}  //  TODO
    // void begin(uint8_t);
    // void begin(int);
    // void end();
    // void setClock(uint32_t);
    // void beginTransmission(uint8_t);
    // void beginTransmission(int);
    // uint8_t endTransmission(void);
    // uint8_t endTransmission(uint8_t);
    // uint8_t requestFrom(uint8_t, uint8_t);
    // uint8_t requestFrom(uint8_t, uint8_t, uint8_t);
	// uint8_t requestFrom(uint8_t, uint8_t, uint32_t, uint8_t, uint8_t);
    // uint8_t requestFrom(int, int);
    // uint8_t requestFrom(int, int, int);
    // virtual size_t write(uint8_t);
    // virtual size_t write(const uint8_t *, size_t);
    // virtual int available(void);
    // virtual int read(void);
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
