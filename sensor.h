//  Defines the common Sensor base class.  Every Sensor is assumed to have a name comprising
//  3 lowercase letters and digits, e.g. "tmp".  The Sensor is capable of producing
//  one, two or three float values as sensor data, in a single sample.  The Sensor instance
//  for specific sensor (e.g. temp_sensor) will provide the function to poll the actual sensor.
//  Every Sensor will also post a SensorMsg to the Network Task or Display Task for
//  aggregation/transmission or for display.
#ifndef SENSOR_H_
#define SENSOR_H_

#include "platform.h"
#include <stdlib.h>
#include <cocoos.h>
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

#define MAX_SENSOR_DATA_SIZE 3  //  Max number of floats that can be returned as sensor data for a single sensor.
#define MAX_SENSOR_NAME_SIZE 3  //  Max number of letters/digits in sensor name.
#define BEGIN_SENSOR_NAME "000"  //  If sensor name is this, then this is the Begin Step that runs at startup.
#define RESPONSE_SENSOR_NAME "RES"  //  This is the response message sent by UART Task to Network Task.

//  Messages sent by Sensor Task containing sensor data will be in this format.
struct SensorMsg {
  Msg_t super;  //  Required for all cocoOS messages.
  char name[MAX_SENSOR_NAME_SIZE + 1];  //  3-character name of sensor e.g. tmp, hmd. Includes terminating null.
  float data[MAX_SENSOR_DATA_SIZE];  //  Array of float sensor data values returned by the sensor.
  uint8_t count;  //  Number of float sensor data values returned by the sensor.
};

//  Interface for getting sensor data, by polling and by events.
struct SensorInfo {
  const char* name;  //  Name of the sensor.
  uint8_t size;      //  How many floats that this sensor will return as sensor data.
  //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  uint8_t (*poll_sensor_func)(float *data, uint8_t size);
  Evt_t *event;  //  Event to be signalled when new sensor data is available.
  uint8_t id;  //  Unique sensor ID.
  uint16_t poll_interval;  //  How often the sensor should be polled, in milliseconds.

  #ifdef __cplusplus
  SensorInfo( //  Constructor for C++
    const char name0[], 
    uint8_t (*poll_sensor_func0)(float *data, uint8_t size));
  #endif // __cplusplus
};

//  Interface for controlling the sensor.
struct SensorControl {
  void (*init_sensor_func)(void);  //  Function for initialising the sensor.
  void (*next_channel_func)(void);  //  TODO: Set sensor to measure next channel.
  void (*prev_channel_func)(void);  //  TODO: Set sensor to measure previous channel.

  #ifdef __cplusplus
  SensorControl(  //  Constructor for C++
    void (*init_sensor_func0)(void),
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
    //  Function for initialising the sensor.
    void (*init_sensor_func)(void),
    //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
    //  Return the number of floats copied.  If no data is available, return 0.
    uint8_t (*poll_sensor_func)(float *data, uint8_t size),
    void (*next_channel_func)(void) = NULL,  //  TODO: Set sensor to measure next channel.
    void (*prev_channel_func)(void) = NULL  //  TODO: Set sensor to measure previous channel.
  );
  #endif // __cplusplus
};

//  Each sensor task will have a Task Data in this format to remember the context of the sensor.
struct SensorContext {
  Sensor *sensor;  //  The sensor for the context.
  uint8_t receive_task_id;  //  Task ID for the task that will receive sensor data, i.e. Network Task or Display Task.
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

//  Copy the received sensor data into the provided data buffer. Return the number of floats copied.
uint8_t receive_sensor_data(
  float *sensorData,  //  Array of floats containing the received sensor data.
  uint8_t sensorDataSize,  //  Number of floats in the received sensor data.
  float *data,  //  Array of floats that sensor data should be copied to.
  uint8_t size  //  Number of floats in the array that sensor data should be copied to.
);

//  Background task to receive and process sensor data.
void sensor_task(void);

END_EXTERN_C  //  End of extern C scope.
#endif  //  SENSOR_H_
