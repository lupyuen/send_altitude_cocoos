//  Stubs to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
#ifndef WIRING_H_
#define WIRING_H_
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  From Arduino.h.  Used by BME280ASpi.h.
#define HIGH 0x1
#define LOW  0x0
#define INPUT 0x0
#define OUTPUT 0x1
#define LSBFIRST 0
#define MSBFIRST 1

//  From Arduino wiring_digital.h
void pinMode(uint8_t, uint8_t);  //  Used by BME280ASpi.h
void digitalWrite(uint8_t pin, uint8_t val);  //  Used by BME280ASpi.h
int digitalRead(uint8_t);  //  Used by BME280SpiSw.cpp
// int analogRead(uint8_t);
// void analogReference(uint8_t mode);
// void analogWrite(uint8_t, int);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  WIRING_H_
