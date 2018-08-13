#include <Arduino.h>
#include "sensor.h"  //  Defines SENSOR_DATA

#ifdef SENSOR_DATA
#include <SPI.h>  //  Needed by BME280 library.

//  Where's the code? Look in main.cpp.
//  Note: setup() and loop() will not be called since main() 
//  is defined (in main.cpp)

//  If you wish, you may use this file as a staging area while
//  porting your Arduino code to .h and .cpp files. That's how
//  bme280.h and bme280.cpp were created.

#endif  //  SENSOR_DATA
