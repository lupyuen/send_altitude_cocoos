//  Defines the common Sensor base class.  We support 2 types of sensors:
//  (1) Polling-Based Sensors: To get sensor data call poll_sensor(), which will block until sensor data is available.
//  (2) Event-Based Sensors: Calling poll_sensor() will return before the sensor data is available.
//      We need to wait for the sensor's semaphore to be signalled.  Then call resume_sensor() to get the sensor data.
//  Event Sensors are better for multitasking than Polling Sensors.  Every Sensor is assumed to have a name comprising
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

#ifdef STM32  //  If STM32 Blue Pill...
//  We call Simulator Module to capture, replay and simulate SPI commands for SPI sensors.
//  We do this so that we can capture the SPI send/receive commands of Arduino sensor drivers and replay
//  them efficiently on STM32, with multitasking.
#include <simulator.h>  //  For Simulator_Control
#include <spiint.h>     //  For SPI_Control
#else  //  If Arduino or other platform...
struct Simulator_Control {};             //  No need to simulate on Arduino.
struct SPI_Control { uint8_t id = 0; };  //  No need for SPI Interface on Arduino.
#endif  //  STM32

BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

#define MAX_SENSOR_DATA_SIZE 3  //  Max number of floats that can be returned as sensor data for a single sensor.
#define MAX_SENSOR_NAME_SIZE 3  //  Max number of letters/digits in sensor name.
#define BEGIN_SENSOR_NAME "000"  //  If sensor name is this, then this is the Begin Step that runs at startup.
#define RESPONSE_SENSOR_NAME "RES"  //  This is the response message sent by UART Task to Network Task.
#define SENSOR_NOT_READY 0xff       //  poll_sensor and resume_sensor functions will return SENSOR_NOT_READY when sensor data is not ready.

//  Messages sent by Sensor Task containing sensor data will be in this format.
struct SensorMsg {
  Msg_t super;                          //  Required for all cocoOS messages.
  char name[MAX_SENSOR_NAME_SIZE + 1];  //  3-character name of sensor e.g. tmp, hmd. Includes terminating null.
  float data[MAX_SENSOR_DATA_SIZE];     //  Array of float sensor data values returned by the sensor.
  uint8_t count;                        //  Number of float sensor data values returned by the sensor.
};

//  Interface for getting sensor data, by polling and by events.
struct SensorInfo {
  #ifdef __cplusplus
  SensorInfo(            //  Constructor for C++
    const char name0[],  //  Name of the sensor.
    //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
    //  Return the number of floats copied.  If no data is available, return 0.
    //  If sensor is not ready to return data, return SENSOR_NOT_READY.
    uint8_t (*poll_sensor_func)(float *data, uint8_t size),
    //  For Event Sensors: Resume the processing of received sensor data.  Copy the received sensor data into the provided data buffer.
    //  Return the number of floats copied.  If no data is available, return 0.
    //  If sensor is not ready to return data, return SENSOR_NOT_READY. Caller should wait for sensor event to be signalled.
    uint8_t (*resume_sensor_func)(float *data, uint8_t size) = NULL,
    //  For Event Sensors: Return true if the processing is complete and new sensor data is available.
    bool (*is_sensor_ready_func)(void) = NULL);
  #endif // __cplusplus

  //  The following fields are set by the sensor.
  const char* name;  //  Name of the sensor.
  uint8_t size;      //  How many floats that this sensor will return as sensor data.
  //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  //  If sensor is not ready to return data, return SENSOR_NOT_READY.
  uint8_t (*poll_sensor_func)(float *data, uint8_t size);
  //  For Event Sensors: Resume the processing of received sensor data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  //  If sensor is not ready to return data, return SENSOR_NOT_READY. Caller should wait for sensor event to be signalled.
  uint8_t (*resume_sensor_func)(float *data, uint8_t size);
  //  For Event Sensors: Return true if the processing is complete and new sensor data is available.
  bool (*is_sensor_ready_func)(void);

  //  The following fields are private to sensor.cpp.
  Sem_t semaphore;         //  Binary Semaphore to be signalled when sensor is ready to return sensor data.
  uint8_t id;              //  Unique sensor ID.
  uint16_t poll_interval;  //  How often the sensor should be polled, in milliseconds.
};

//  Interface for controlling the sensor.
struct SensorControl {
  #ifdef __cplusplus
  SensorControl(                       //  Constructor for C++
    void (*init_sensor_func)(void),    //  Function for initialising the sensor.
    void (*next_channel_func)(void),   //  TODO: Set sensor to measure next channel.
    void (*prev_channel_func)(void));  //  TODO: Set sensor to measure previous channel.
  #endif // __cplusplus

  void (*init_sensor_func)(void);   //  Function for initialising the sensor.
  void (*next_channel_func)(void);  //  TODO: Set sensor to measure next channel.
  void (*prev_channel_func)(void);  //  TODO: Set sensor to measure previous channel.
};

//  Interface for accessing the sensor data and controlling a sensor.
struct Sensor {  
  #ifdef __cplusplus
  Sensor(               //  Constructor for C++
    const char name[],  //  Name of sensor.
    //  Function for initialising the sensor.
    void (*init_sensor_func)(void),
    //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
    //  Return the number of floats copied.  If no data is available, return 0.
    //  If sensor is not ready to return data, return SENSOR_NOT_READY.
    uint8_t (*poll_sensor_func)(float *data, uint8_t size),
    //  For Event Sensors: Resume the processing of received sensor data.  Copy the received sensor data into the provided data buffer.
    //  Return the number of floats copied.  If no data is available, return 0.
    //  If sensor is not ready to return data, return SENSOR_NOT_READY. Caller should wait for sensor event to be signalled.
    uint8_t (*resume_sensor_func)(float *data, uint8_t size) = NULL,
    //  For Event Sensors: Return true if the processing is complete and new sensor data is available.
    bool (*is_sensor_ready_func)(void) = NULL,
    void (*next_channel_func)(void) = NULL,  //  TODO: Set sensor to measure next channel.
    void (*prev_channel_func)(void) = NULL   //  TODO: Set sensor to measure previous channel.
  );
  #endif // __cplusplus

  SensorInfo info;              //  For accessing sensor data
  SensorControl control;        //  For controlling the sensor
  uint32_t port_id;             //  For the port ID used by sensor e.g. SPI1, I2C1
  SPI_Control *port;            //  For SPI port used by sensor
  Simulator_Control simulator;  //  For simulating the sensor.  Must be in static memory, not stack memory.
};

//  Each sensor task will have a Task Data in this format to remember the context of the sensor.
struct SensorContext {
  Sensor *sensor;           //  The sensor for the context.
  uint8_t receive_task_id;  //  Task ID for the task that will receive sensor data, i.e. Network Task or Display Task.
  SensorMsg msg;            //  Temporary space for composing the sensor data message.
  Sem_t *read_semaphore;    //  If set, wait for this Counting Semaphore before reading sensor data.  Used for controlling access to I/O ports e.g. SPI, I2C.
  Sem_t *send_semaphore;    //  If set, wait for this Binary Semaphore before sending data.  Used for waiting until sensor data is available, or until the Simulator has replayed an SPI command.
};

//  Set up the sensor context. Allocate a new sensor ID and sensor semaphore.
void setup_sensor_context(
  SensorContext *context,  //  Context to be set up.
  Sensor *sensor,          //  Sensor to be set up.
  uint16_t pollInterval,   //  Polling interval in milliseconds.
  uint8_t displayTaskID    //  Task ID for the Network or Display Task.  This task will receive sensor data messages.
);

//  Copy the received sensor data into the provided data buffer. Return the number of floats copied.
uint8_t receive_sensor_data(
  float *sensorData,       //  Array of floats containing the received sensor data.
  uint8_t sensorDataSize,  //  Number of floats in the received sensor data.
  float *data,             //  Array of floats that sensor data should be copied to.
  uint8_t size             //  Number of floats in the array that sensor data should be copied to.
);

//  Background task to receive and process sensor data.
void sensor_task(void);

END_EXTERN_C  //  End of extern C scope.
#endif  //  SENSOR_H_
