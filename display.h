#ifndef DISPLAY_H_
#define DISPLAY_H_

#include <stdint.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++

#ifdef __cplusplus ////
extern "C" {
#endif ////

// Message signals
#define TEMP_DATA 32
#define GYRO_DATA 33
#define DISPLAY_MSG 34

#define sensorDataSize 3  //  Max number of floats that can be returned as sensor data for a single sensor.
#define sensorNameSize 3  //  Max number of letters/digits in sensor name.
#define sensorDisplaySize 4  //  Max number of sensors that can be displayed during a single refresh.

/**
 * Message type for display task
 * The value of super.signal indicates which type of data,
 * (temp_sensor or gyro data in this case)
 */
typedef struct {
  Msg_t super;
  char name[sensorNameSize + 1];  //  3-character name of sensor e.g. tmp, hmd. Includes terminating null.
  float data[sensorDataSize];  //  Array of float sensor data values returned by the sensor.
  uint8_t count;  //  Number of float sensor data values returned by the sensor.
} DisplayMsg;

typedef struct {
  void (*refresh_func)(void);
  void (*update_data_func)(uint8_t id, const char *name, const float *data, uint8_t count);
} Display;

Display *get_display(void);
void init_display(void);
void display_task(void);

#ifdef __cplusplus ////
}
#endif ////

#endif /* DISPLAY_H_ */
