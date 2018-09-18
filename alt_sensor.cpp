//  Instance of Sensor that reads the altitude sensor in BME280 I2C.
//  Caller must use a semaphore to prevent concurrent access to BME280 
//  module, which runs on a single I2C Bus.
#include "platform.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"

#ifdef SENSOR_DATA
#include "bme280.h"
#include "alt_sensor.h"
#include <EnvironmentCalculations.h>

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(float *data, uint8_t size);

//  Number of floats that this sensor returns as sensor data.
#define sensorDataSize 1

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "alt",                //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,         //  Function for initialising the sensor.
  &poll_sensor          //  Function for polling sensor data.
);

static SensorContext sensorContext;  //  Remembers the sensor context.
static float sensorData[sensorDataSize];  //  Array of floats for remembering the sensor data.

static void init_sensor(void) {
  //  Initialise the sensor if necessary. Assume sensor and sensorContext objects have been populated.
  sensor.port_id = BME280_SENSOR_PORT;      //  Connect to the sensor at this port.
  sensor.port = spi_setup(sensor.port_id);  //  Get the SPI port.
  if (sensor.port) { debug_print(sensor.info.name); debug_print(" >> config spi"); debug_print((int) sensor.port->id); debug_println(" arduino library"); debug_flush(); }
  bme280_setup(sensor.port_id);             //  Set up the BME280 API.
}

static uint8_t poll_sensor(float *data, uint8_t size) {
  //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  debug(sensor.info.name, F(" >> poll_sensor"));
  
  //  Read sensor data from BME280.
  static BME280::PresUnit presUnit(BME280::PresUnit_Pa);
  float pressure = bme.pres(presUnit);  //  Get air pressure in Pa.
  static EnvironmentCalculations::AltitudeUnit envAltUnit = EnvironmentCalculations::AltitudeUnit_Meters;
  float altitude = EnvironmentCalculations::Altitude(
    pressure, envAltUnit);  //  Compute altitude in metres above sea level.
  sensorData[0] = altitude;

  //  Simulated sensor.
  //// sensorData[0] = 12.3 + rand() % 10;

  //  Copy the received sensor data and return the number of floats copied.
  return receive_sensor_data(sensorData, sensorDataSize, data, size);
}

SensorContext *setup_alt_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

#endif  //  SENSOR_DATA
