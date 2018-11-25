//  Instance of Sensor that reads the pressue gauge sensor via port ADC1.
#include "platform.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"

#ifdef SENSOR_DATA
#include <adcint.h>
#include "pressure_sensor.h"

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(float *data, uint8_t size);

//  Number of floats that this sensor returns as sensor data.
#define sensorDataSize 1

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "prs",                //  Name of sensor. The Structured Message field will use this name.
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
  int vref = adc_read_scaled_vref();
	int adc0 = adc_read(0) * 330 / 4095;
	int adc1 = adc_read(1) * 330 / 4095;
	int temp = adc_read_scaled_temperature();
  sensorData[0] = adc0;

  //  Dump the sensor values.
  debug_print(sensor.info.name);
  debug_print(" adc0 "); debug_print(adc0);
  debug_print(", adc1 "); debug_print(adc1);
  debug_print(", temp "); debug_print(temp);
  debug_print(", vref "); debug_print(vref);
  debug_println("");

  //  Simulated sensor.
  //// sensorData[0] = 12.3 + rand() % 10;

  //  Copy the received sensor data and return the number of floats copied.
  return receive_sensor_data(sensorData, sensorDataSize, data, size);
}

SensorContext *setup_pressure_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

#endif  //  SENSOR_DATA
