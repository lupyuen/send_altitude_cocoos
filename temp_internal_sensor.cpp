//  Instance of Sensor that reads the STM32 Blue Pill internal temperature sensor via port ADC1.
#include "platform.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"

#ifdef SENSOR_DATA
#include <adcint.h>
#include "temp_internal_sensor.h"

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(float *data, uint8_t size);

//  Number of floats that this sensor returns as sensor data.
#define sensorDataSize 1

//  Construct a sensor object with the sensor functions.
//  Warning: Must not be used with any other temperature sensor, because "tmp" sensor name will clash.
static Sensor sensor(
  "tmp",                //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,         //  Function for initialising the sensor.
  &poll_sensor          //  Function for polling sensor data.
);

static SensorContext sensorContext;  //  Remembers the sensor context.
static float sensorData[sensorDataSize];  //  Array of floats for remembering the sensor data.

static void init_sensor(void) {
  //  Initialise the sensor if necessary. Assume sensor and sensorContext objects have been populated.
  sensor.port_id = ADC1;  //  Connect to the sensor at this port.
  adc_setup();            //  Get the ADC port.
}

static uint8_t poll_sensor(float *data, uint8_t size) {
  //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  debug(sensor.info.name, F(" >> poll_sensor"));
  
  //  Read sensor data from ADC.
	float temp = adc_read_scaled_temperature() / 100.0;
  sensorData[0] = temp;

  //  Dump the sensor values.
  debug_print(sensor.info.name);
  debug_print(" >> temp "); debug_print(temp);
  debug_println("");

  //  Simulated sensor.
  //// sensorData[0] = 12.3 + rand() % 10;

  //  Copy the received sensor data and return the number of floats copied.
  return receive_sensor_data(sensorData, sensorDataSize, data, size);
}

SensorContext *setup_temperature_internal_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

#endif  //  SENSOR_DATA
