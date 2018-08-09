#ifndef DISPLAY_H_
#define DISPLAY_H_

#include <stdint.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define sensorDisplaySize 4  //  Max number of sensors that can be displayed during a single refresh.
#define displayMsgPoolSize 6  //  Allow only 6 display messages to be queued, which means fewer than 6 sensors allowed.
#define DISPLAY_MSG 34  //  TODO: Signal the display to update.

//  Messages sent to Display Task will be in this format.
struct DisplayMsg {
  Msg_t super;  //  Required for all cocoOS messages.
  char name[sensorNameSize + 1];  //  3-character name of sensor e.g. tmp, hmd. Includes terminating null.
  float data[sensorDataSize];  //  Array of float sensor data values returned by the sensor.
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
#endif  //  DISPLAY_H_
