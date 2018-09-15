//  Instance of Sensor that reads the BME280 temperature sensor connected via SPI interface.
//  This is an example of an Event-based Sensor.
#include "platform.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"

#if defined(SENSOR_DATA) && defined(STM32)  //  Only for STM32.
#include <spiint.h>
#include "temp_event_sensor.h"

//  These are the sensor functions that we will implement in this file.
static void init_sensor(void);
static uint8_t poll_sensor(float *data, uint8_t size);
static uint8_t resume_sensor(float *data, uint8_t size);
static bool poll_sensor(void);

//  Number of floats that this sensor returns as sensor data.
#define sensorDataSize 1

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "tme",                //  Name of sensor. The Structured Message field will use this name.
  &init_sensor,         //  Function for initialising the sensor.
  &poll_sensor,         //  Function for polling sensor data.
  &resume_sensor,       //  For Event Sensors: Function for resuming the processing of received sensor data.
  &is_sensor_ready      //  For Event Sensors: Function for checking whether the processing is complete and new sensor data is available.
);

static SensorContext sensorContext;  //  Remembers the sensor context.
static float sensorData[sensorDataSize];  //  Array of floats for remembering the sensor data.

static void init_sensor(void) {
  //  Initialise the sensor port. sensor and sensorContext objects have been populated.
  sensor.port = spi_setup(TEMP_EVENT_SENSOR_PORT);  //  Get the SPI port.
  if (sensor.port == NULL) { return; }  //  Quit if the setup failed.

  //  Configure the SPI port, specific to the BME280 sensor.  Change this for other sensors.
  spi_configure(
    sensor.port,  //  SPI port to be configured.
    500 * kHz,    //  Maximum speed supported by the sensor.
    MSBFIRST,     //  Bit order: MSBFIRST or LSBFIRST.
    SPI_MODE0);   //  Data mode: SPI_MODE0, 1, 2 or 3.
}

//  TODO
/*
tmp >> replayed >> 72 << ff
tmp >> replayed >> 01 << ff
tmp >> replayed >> 74 << ff
tmp >> replayed >> 25 << ff
tmp >> replayed >> 75 << ff
tmp >> replayed >> a0 << ff
tmp >> replayed >> f7 << ff
tmp >> replayed >> 00 << 51
tmp >> replayed >> 00 << 12
tmp >> replayed >> 00 << 00
tmp >> replayed >> 00 << 82
tmp >> replayed >> 00 << dd
tmp >> replayed >> 00 << 00
tmp >> replayed >> 00 << 7f
*/
#define TX_LEN 16
#define RX_LEN 16
static uint8_t tx_buf[TX_LEN];  //  Must be in static memory not stack memory because it will be used for DMA.
static uint8_t rx_buf[RX_LEN];  //  Must be in static memory not stack memory because it will be used for DMA.

static uint8_t poll_sensor(float *data, uint8_t size) {
  //  For Event Sensors: Poll the sensor for new data and return SENSOR_NOT_READY.
  //  Event Task will wait for the sensor event to be signalled and call resume_sensor().
  debug(sensor.info.name, F(" >> poll_sensor"));
  
  //  Send the SPI transceive command to read sensor data from BME280.  Sensor event will be signalled when done.
  SPI_Fails result = spi_transceive(sensor.port, tx_buf, TX_LEN, rx_buf, RX_LEN, sensor.info.event);
  if (result != SPI_Ok) { return 0; }  //  An error has occurred.  Stop the processing.

  //  Return SENSOR_NOT_READY so that Event Task will wait for the sensor event to be signalled and call resume_sensor().
  return SENSOR_NOT_READY;
}

static uint8_t resume_sensor(float *data, uint8_t size) {
  //  For Event Sensors: Resume the processing of received sensor data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  //  If sensor is not ready to return data, return SENSOR_NOT_READY. Event Task will wait for sensor event to be signalled
  //  and call resume_sensor() again.
  debug(sensor.info.name, F(" >> resume_sensor"));
  
  //  If SPI transceive command has not been completed, return SENSOR_NOT_READY so that Event Task will wait for the sensor event to be signalled and call resume_sensor() again.
  if (!is_sensor_ready()) { return SENSOR_NOT_READY; }
  if (!spi_is_transceive_successful(sensor.port)) { return 0; }  //  An error occurred.  Stop the processing.

  //  Process the received SPI data into sensor data.
  //  TODO: sensorData[0] = bme.temp(tempUnit);  //  Get temperature in Celsius.

  //  Simulated sensor.
  sensorData[0] = 12.3 + rand() % 10;

  //  Copy the received sensor data and return the number of floats copied.
  return receive_sensor_data(sensorData, sensorDataSize, data, size);
}

static bool is_sensor_ready(void) {
  //  For Event Sensors: Return true if the processing is complete and new sensor data is available.
  //  We check whether the SPI transceive is complete.
  return spi_is_transceive_complete(sensor.port);
}

SensorContext *setup_temp_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

#endif  //  SENSOR_DATA && STM32
