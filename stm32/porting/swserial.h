//  The code here reproduces the Arduino class SoftwareSerial.
//  To be removed when we no longer use SoftwareSerial class.
//  Used by uart.cpp.
#ifndef SWSERIAL_H_
#define SWSERIAL_H_
#ifdef __cplusplus  //  SoftwareSerial class for C++ only.
#include "serial.h"

class SoftwareSerial: public Print {  //  TODO: Handle UART Task.
public:
  SoftwareSerial(unsigned rx, unsigned tx): Print(rx, tx) {}
};

#endif  //  __cplusplus
#endif  //  SWSERIAL_H_
