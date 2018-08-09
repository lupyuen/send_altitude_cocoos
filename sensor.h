#ifndef SENSOR_H_
#define SENSOR_H_

#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#define sensorDataSize 3  //  Max number of floats that can be returned by a sensor as sensor data.
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

//  Interface for getting sensor data, by polling and by events.
struct SensorInfo {
  const char* name;  //  Name of the sensor.
  //  Poll for new sensor data. If new sensor data is available,
  //  return the number of float values available.  Else return 0.
  uint8_t (*poll_sensor_func)(void);
  //  Fetch the sensor data into the data buffer, which should be
  //  a pointer to float, or an array of floats.  size is the max
  //  number of floats that the function can copy into the buffer.
  //  Returns number of floats copied.
  uint8_t (*receive_sensor_data_func)(float *data, uint8_t size);
  Evt_t *event;  //  Event signalled when new sensor data is available.
  uint8_t id;  //  Unique sensor ID.
  uint16_t poll_interval;  //  How often the sensor should be polled, in milliseconds.

  #ifdef __cplusplus
  SensorInfo( //  Constructor for C++
    const char name0[], 
    uint8_t (*poll_sensor_func0)(void),
    uint8_t (*receive_sensor_data_func0)(float *data, uint8_t size));
  #endif // __cplusplus
};

//  Interface for controlling the sensor.
struct SensorControl {
  //  Function for initialising the sensor with the specified sensor ID and
  //  polling interval. event will be triggered when sensor data is available.
  void (*init_sensor_func)(uint8_t id, Evt_t *event, uint16_t poll_interval);
  void (*next_channel_func)(void);  //  TODO: Set sensor to measure next channel.
  void (*prev_channel_func)(void);  //  TODO: Set sensor to measure previous channel.

  #ifdef __cplusplus
  SensorControl(  //  Constructor for C++
    void (*init_sensor_func0)(uint8_t id, Evt_t *event, uint16_t poll_interval),
    void (*next_channel_func0)(void),
    void (*prev_channel_func0)(void));
  #endif // __cplusplus
};

//  Interface for accessing the sensor data and controlling a sensor.
struct Sensor {  
  SensorInfo info; //  For accessing sensor data
  SensorControl control; //  For controlling the sensor

  #ifdef __cplusplus
  Sensor(  //  Constructor for C++
    const char name[],  //  Name of sensor.
    //  Function for initialising the sensor with the specified sensor ID and
    //  polling interval. event will be triggered when sensor data is available.
    void (*init_sensor_func)(uint8_t id, Evt_t *event, uint16_t poll_interval),
    //  Poll for new sensor data. If new sensor data is available,
    //  return the number of float values available.  Else return 0.
    uint8_t (*poll_sensor_func)(void),
    //  Fetch the sensor data into the data buffer, which should be
    //  a pointer to float, or an array of floats.  size is the max
    //  number of floats that the function can copy into the buffer.
    //  Returns number of floats copied.
    uint8_t (*receive_sensor_data_func)(float *data, uint8_t size),
    void (*next_channel_func)(void),  //  TODO: Set sensor to measure next channel.
    void (*prev_channel_func)(void)  //  TODO: Set sensor to measure previous channel.
  );
  #endif // __cplusplus
};

//  Each sensor task will have a Task Data in this format to remember the sensor context.
struct SensorContext {
  Sensor *sensor;  //  The sensor for the context.
  float data[sensorDataSize];  //  Array of float sensor data values returned by the sensor.
  uint8_t count;  //  Number of float sensor data values returned by the sensor.
  uint8_t display_task_id;  //  Task ID for the Display Task.  Used for sending display messages.
  Evt_t event;  //  TODO: Event that will be triggered when sensor data is available.
};

//  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
extern Sem_t i2cSemaphore;

//  Set up the sensor context. Allocate a new sensor ID and event.
void setup_sensor_context(
  SensorContext *context,  //  Context to be set up.
  Sensor *sensor,  //  Sensor to be set up.
  uint16_t pollInterval,  //  Polling interval in milliseconds.
  uint8_t displayTaskID  //  Task ID for the Display Task.  Used for sending display messages.
);

//  Background task to receive and process sensor data.
void sensor_task(void);

//  Print a message to the Arduino serial console. Defined in main.cpp.
void debug(
  const char *s1,  //  Text to be printed.
  const char *s2  //  Another text to be printed.
  #ifdef __cplusplus
    = 0  //  Second parameter may be omitted.
  #endif
  );

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#endif  //  SENSOR_H_
