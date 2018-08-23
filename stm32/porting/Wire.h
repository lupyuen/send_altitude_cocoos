//  Expose the Arduino Wire I2C interface for BME280 library.
#ifndef WIRE_H_
#define WIRE_H_
#include <i2cint.h>  //  For I2CInterface
#ifdef __cplusplus  //  I2CInterface class for C++ only

extern I2CInterface Wire;  //  Used by BME280 library.

#endif  //  __cplusplus
#endif  //  WIRE_H_
