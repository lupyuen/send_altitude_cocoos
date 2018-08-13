#ifndef DISPLAY_H_
#define DISPLAY_H_

//  Uncomment to log sensor data to console via Display Task instead of sending to network.
//  #define SENSOR_DISPLAY

#include "platform.h"
#include <stdint.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"  //  Defines maxSensorNameSize and maxSensorDataSize

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define SERIAL_BAUD 9600  //  Serial Monitor will run at this bitrate.
#define sensorDisplaySize 4  //  Max number of sensors that can be displayed during a single refresh.
#define displayMsgPoolSize 6  //  Allow only 6 display messages to be queued, which means fewer than 6 sensors allowed.
#define DISPLAY_MSG 34  //  TODO: Signal the display to update.

//  Messages sent to Display Task will be in this format.
struct DisplayMsg {
  Msg_t super;  //  Required for all cocoOS messages.
  char name[maxSensorNameSize + 1];  //  3-character name of sensor e.g. tmp, hmd. Includes terminating null.
  float data[maxSensorDataSize];  //  Array of float sensor data values returned by the sensor.
  uint8_t count;  //  Number of float sensor data values returned by the sensor.
};

//  Interface for displaying sensor data.
struct Display {
  void (*refresh_func)(void);
  void (*update_data_func)(uint8_t id, const char *name, const float *data, uint8_t count);
};

Display *get_display(void);  //  Return the global instance of the display interface.
void init_display(void);  //  Initialise the display interface.
void display_task(void);  //  Display Task runs this function to display messages received.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

//  Print a message to the Arduino serial console.  The function is overloaded to support
//  printing of strings in dynamic memory and strings in flash (e.g. F(...)).
void debug(
  const char *s1,  //  String in dynamic memory.
  const char *s2  //  Another string in dynamic memory.
  #ifdef __cplusplus
    = 0  //  Second parameter may be omitted.
  #endif
  );

#ifdef __cplusplus  //  Overloaded functions for C++ only, not C.
#ifdef ARDUINO  //  Flash memory for Arduino only.

void debug(
  const __FlashStringHelper *s1  //  String in flash memory e.g. F("the string")
);

void debug(
  const char *s1,  //  String in dynamic memory.
  const __FlashStringHelper *s2  //  String in flash memory e.g. F("the string")
);

void debug(
  const __FlashStringHelper *s1,  //  String in flash memory e.g. F("the string")
  const char *s2  //  String in dynamic memory.
);

#endif  //  ARDUINO
#endif  //  __cplusplus

#endif  //  DISPLAY_H_
