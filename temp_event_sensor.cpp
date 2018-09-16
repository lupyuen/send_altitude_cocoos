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
static bool is_sensor_ready(void);

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

/*  These are the actual SPI bytes sent and received, captured by the Simulator while running the BME280Spi.cpp code.
send >> 72, receive << ff
send >> 01, receive << ff
send >> 74, receive << ff
send >> 25, receive << ff
send >> 75, receive << ff
send >> a0, receive << ff
send >> f7, receive << ff
send >> 00, receive << 51
send >> 00, receive << 12
send >> 00, receive << 00
send >> 00, receive << 82
send >> 00, receive << dd
send >> 00, receive << 00
send >> 00, receive << 7f */

//  Number of bytes to send and receive.
#define TX_LEN 14
#define RX_LEN 14

//  SPI bytes to be sent.  These bytes will write and update BME280 registers.
static uint8_t tx_buf[TX_LEN] = {  //  Must be in static memory not stack memory because it will be used for DMA.
  0x72,
  0x01,
  0x74,
  0x25,
  0x75,
  0xa0,
  0xf7,  //  Read the registers.
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

//  Buffer for receiving SPI bytes.
static uint8_t rx_buf[RX_LEN];  //  Must be in static memory not stack memory because it will be used for DMA.

//  BME280 calibration data.  For simplicity, we hardcode here.
static uint8_t m_dig[] = { 0x97, 0x6e, 0xe6, 0x65, 0x32, 0x00 };

static uint8_t poll_sensor(float *data, uint8_t size) {
  //  For Event Sensors: Poll the sensor for new data and return SENSOR_NOT_READY.
  //  Event Task will wait for the sensor event to be signalled and call resume_sensor().
  debug(sensor.info.name, F(" >> poll_sensor"));
  
  //  Open the SPI port.
  SPI_Fails result = spi_open(sensor.port);
  if (result != SPI_Ok) { return 0; }  //  An error has occurred.  Stop the processing.

  //  Send the SPI transceive command to read sensor data from BME280.  Sensor event will be signalled when done.
  result = spi_transceive(sensor.port, tx_buf, TX_LEN, rx_buf, RX_LEN, NULL, &sensor.info.semaphore);
  if (result != SPI_Ok) {    //  An error has occurred.  Stop the processing.
    spi_close(sensor.port);  //  Close the SPI port.
    return 0; 
  }

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

  //  Close the SPI port.
  spi_close(sensor.port);  
  if (!spi_is_transceive_successful(sensor.port)) { return 0; }  //  An error occurred.  Stop the processing.

  //  Convert the received SPI data in rx_buf to temperature.  Based on https://github.com/finitespace/BME280/blob/master/src/BME280.cpp
  debug_print(sensor.info.name); spi_dump_packet(sensor.port);
  uint8_t *rx_data = &rx_buf[7];  //  Response starts at the 8th byte.
  uint32_t raw = ((uint32_t) rx_data[3] << 12) | ((uint32_t) rx_data[4] << 4) | ((uint32_t) rx_data[5] >> 4);
  int32_t var1, var2, final;
  uint16_t  dig_T1 = (m_dig[1] << 8) | m_dig[0];
  int16_t   dig_T2 = (m_dig[3] << 8) | m_dig[2];
  int16_t   dig_T3 = (m_dig[5] << 8) | m_dig[4];
  var1 = ((((raw >> 3) - ((int32_t)dig_T1 << 1))) * ((int32_t)dig_T2)) >> 11;
  var2 = (((((raw >> 4) - ((int32_t)dig_T1)) * ((raw >> 4) - ((int32_t)dig_T1))) >> 12) * ((int32_t)dig_T3)) >> 14;
  int32_t t_fine = var1 + var2;
  final = (t_fine * 5 + 128) >> 8;
  const bool useCelsius = true;

  //  Save the computed temperature as sensor data.
  sensorData[0] = useCelsius ? final/100.0 : final/100.0*9.0/5.0 + 32.0;

  //  Simulated sensor.
  //  sensorData[0] = 12.3 + rand() % 10;

  //  Copy the received sensor data and return the number of floats copied.
  return receive_sensor_data(sensorData, sensorDataSize, data, size);
}

static bool is_sensor_ready(void) {
  //  For Event Sensors: Return true if the processing is complete and new sensor data is available.
  //  We check whether the SPI transceive is complete.
  return spi_is_transceive_complete(sensor.port);
}

SensorContext *setup_temp_event_sensor(
  uint16_t pollInterval,
  uint8_t displayTaskID
  ) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, displayTaskID);
  return &sensorContext;
}

#endif  //  SENSOR_DATA && STM32
