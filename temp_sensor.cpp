//  Implements the temperature sensor for BME280. Caller must use a semaphore to
//  prevent concurrent access to BME280 module, which runs on a single I2C Bus.
#include "platform.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"

#ifdef SENSOR_DATA
#include "bme280.h"
#include "temp_sensor.h"

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(float *data, uint8_t size);

//  Number of floats that this sensor returns as sensor data.
#define sensorDataSize 2

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "tmp",                //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,         //  Function for initialising the sensor.
  &poll_sensor          //  Function for polling sensor data.
);

static SensorContext sensorContext;  //  Remembers the sensor context.
static float sensorData[sensorDataSize];  //  Array of floats for remembering the sensor data.

static void init_sensor(void) {
  //  Initialise the sensor if necessary. sensor and sensorContext objects have been populated.
  bme280_setup();  //  Set up the BME280 API.
}



static float latitude[] = { 57.6345030,
                            57.6458880,
                            57.6459920,
                            57.6744700,
                            57.6996480,
                            57.7035180,
                            57.7035180,
                            57.7087050
                          };

static float longitude[] = {11.9578160,
                            11.9518800,
                            11.9329300,
                            11.9393590,
                            11.9609030,
                            11.9639790,
                            11.9639790,
                            11.9634110
                            };

static uint8_t poll_sensor(float *data, uint8_t size) {
  //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  debug(sensor.info.name, F(" >> poll_sensor")); ////
  
  //  Read sensor data from BME280.
  static BME280::TempUnit tempUnit(BME280::TempUnit_Celsius);
  sensorData[0] = bme.temp(tempUnit);  //  Get temperature in Celsius.

  static uint32_t cnt = 0;
  static uint8_t index = 0;

  if (cnt++ == (12*15)) {
    index = (index + 1) % 8;
    cnt = 0;
  }

  sensorData[0] =  latitude[index];
  sensorData[1] =  longitude[index];

  //sensorData[0] = 57.6345030;
  //sensorData[1] =  11.9578160;

  //  Simulated sensor.
  //// sensorData[0] = 12.3 + rand() % 10;

  //  Copy the received sensor data and return the number of floats copied.
  return receive_sensor_data(data, size, sensorData, sensorDataSize);
}

SensorContext *setup_temp_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

#endif  //  SENSOR_DATA
