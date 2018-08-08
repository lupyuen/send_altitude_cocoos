#ifndef SENSOR_H_
#define SENSOR_H_

#include <cocoos-cpp.h>  //  TODO: Workaround for cocoOS in C++

#define sensorDataSize 3  //  Max number of floats that can be returned by a sensor as sensor data.

#ifdef __cplusplus ////
extern "C" {
#endif ////

/**
* Information interface
*/
struct SensorInfo {
  /**
   *  Name of the sensor
   */
  const char* name;

  /**
   *  Poll for new data
   *  @return 1 if new data available, 0 otherwise
   */
  uint8_t (*poll_sensor_func)(void);

  /**
   * Get sensor data
   * @param buf, pointer to output buffer
   * @param size, size of buffer
   * @return number of bytes copied
   */
  uint8_t (*receive_sensor_data_func)(float *data, uint8_t size);

  /**
   *  Event signaled by driver when new data available
   *  Should be set by the application during startup using the init() function
   */
  Evt_t *event;

  /*
   * Sensor id
   * Could be used as message id. Should be set by the init() method.
   */
  uint8_t id;

  /**
   *  Minimum polling interval, set to 0 if polling is not used
   *  If we know that a sensor is updated every second, it would be
   *  meaningless to poll it more often. Set the value to indicate
   *  a suitable minimum period.
   */
  uint16_t poll_interval;

  #ifdef __cplusplus
  //  Constructor for C++
  SensorInfo(
    const char name0[],
    uint8_t (*poll_sensor_func0)(void),
    uint8_t (*receive_sensor_data_func0)(float *data, uint8_t size)
  ) {
    name = name0;
    poll_sensor_func = poll_sensor_func0;
    receive_sensor_data_func = receive_sensor_data_func0;
  }
  #endif // __cplusplus
};

/**
 * Control interface
 */
struct SensorControl {
  /**
   * Initialize sensor
   * Should be called during main startup
   *
   * @param id, an unique id for the sensor
   * @param event, event that should be signaled. Set to 0 if not used.
   * @param period_ms, minimum polling interval, set to 0 if not used.
   */
  void (*init_sensor_func)(uint8_t id, Evt_t *event, uint16_t poll_interval);

  /**
   * Set sensor to measure next channel
   */
  void (*next_channel_func)(void);

  /**
   * Set sensor to measure previous channel
   */
  void (*prev_channel_func)(void);

  #ifdef __cplusplus
  //  Constructor for C++
  SensorControl(
    void (*init_sensor_func0)(uint8_t id, Evt_t *event, uint16_t poll_interval),
    void (*next_channel_func0)(void),
    void (*prev_channel_func0)(void)
  ) {
    init_sensor_func = init_sensor_func0;
    next_channel_func = next_channel_func0;
    prev_channel_func = prev_channel_func0;
  }
  #endif // __cplusplus
};

/*
 * Sensor interface
 * Used by tasks to access a sensor
 * An example could be an i2c connected sensor. The sensor driver
 * could signal the event in the tx interrupt when new data is available,
 * or return 1 in the poll() function.
 */
struct Sensor {  
  SensorInfo info; //  Information interface  
  SensorControl control; //  Control interface
  #ifdef __cplusplus
  //  Constructor for C++
  Sensor(
    const char name[],
    void (*init_sensor_func)(uint8_t id, Evt_t *event, uint16_t poll_interval),
    uint8_t (*poll_sensor_func)(void),
    uint8_t (*receive_sensor_data_func)(float *data, uint8_t size),
    void (*next_channel_func)(void),
    void (*prev_channel_func)(void)
  );
  #endif // __cplusplus
};

//  Task Data used by sensor tasks to remember the sensor context.
struct SensorContext {
  Sensor *sensor;  //  The sensor for the context.
  float data[sensorDataSize];  //  Array of float sensor data values returned by the sensor.
  uint8_t count;  //  Number of float sensor data values returned by the sensor.
  uint8_t display_task_id;  //  Task ID for the Display Task.  Used for sending display messages.
  Evt_t event;  //  TODO: Event that will be triggered when sensor data is available.
};

//  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
extern Sem_t i2cSemaphore;

//  Setup the sensor context.
void setup_sensor_context(
  SensorContext *context,
  Sensor *sensor,
  uint8_t id,
  uint16_t pollInterval,
  uint8_t displayTaskID
  );

//  Background task to receive and process sensor data.
void sensor_task(void);

//  Print a message to the Arduino serial console.
void debug(const char *s1, 
  const char *s2
  #ifdef __cplusplus
    = 0
  #endif
  );

#ifdef __cplusplus ////
}
#endif ////

#endif /* SENSOR_H_ */

