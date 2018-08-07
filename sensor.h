#ifndef SENSOR_H_
#define SENSOR_H_

#include <cocoos.h>

#ifdef __cplusplus ////
extern "C" {
#endif ////

/*
 * Sensor interface
 * Used by tasks to access a sensor
 * An example could be an i2c connected sensor. The sensor driver
 * could signal the event in the tx interrupt when new data is available,
 * or return 1 in the poll() function.
 */
 
//// For Arduino: Break into SensorInfo, Control_Info_t structs for easier initialisation in C++
/**
* Information interface
*/
typedef struct {
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
  // uint8_t (*receive_sensor_data)(uint8_t *buf, uint8_t size);  // receive max size bytes into buf
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

} SensorInfo;
////

////
/**
 * Control interface
 */
typedef struct {
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
} SensorControl;
////
  
typedef struct {  
  SensorInfo info; //  Information interface  
  SensorControl control; //  Control interface
} Sensor;

//  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
extern Sem_t i2cSemaphore;

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
