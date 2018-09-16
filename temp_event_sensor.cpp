//  Instance of Sensor that reads the BME280 temperature sensor connected via SPI interface.
//  This is an example of an Event-based Sensor.
#include "platform.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"

#if defined(SENSOR_DATA) && defined(STM32)  //  Event Sensors are only supported on STM32, because Arduino doesn't allow DMA.
#include <spiint.h>
#include "temp_event_sensor.h"

/*  These are the actual SPI bytes sent and received, captured by the Simulator while running the BME280Spi.cpp code:
    https://github.com/finitespace/BME280/blob/master/src/BME280Spi.cpp
    This BME280Spi.cpp capture was used to create the tx_buf data below that we will actually send.  Note the original
    BME280Spi.cpp capture sends and receives one byte at a time, whereas the code below sends all 15 bytes in a single burst
    and waits for 15 bytes to be received.  Check the BME280 datasheet for explanation of the registers:
    https://cdn.sparkfun.com/assets/learn_tutorials/4/1/9/BST-BME280_DS001-10.pdf
    Note that Write Register commands always clear the highest bit (e.g. "Write Register 0xF2" becomes "0x72").
    Read Register commands always set the highest bit (e.g. "Read Register 0xF7" becomes "0xF7").

    send >> 72, receive << ff - Send command to write to BME280 Register 0xF2 “ctrl_hum”
    send >> 01, receive << ff - Set the humidity data acquisition options to 0x01
    send >> 74, receive << ff - Send command to write to BME280 Register 0xF4 “ctrl_meas”
    send >> 25, receive << ff - Set the pressure and temperature data acquisition options to 0x25
    send >> 75, receive << ff - Send command to write to BME280 Register 0xF5 “config”
    send >> a0, receive << ff - Set the rate, filter and interface options to 0xA0
    send >> f7, receive << ff - Send command to burst read from BME280 Registers 0xF7 to 0xFE (“press”, “temp”, “hum”)
    send >> 00, receive << 51 - Receive Register 0xF7 (raw pressure MSB = 0x51)
    send >> 00, receive << 96 - Receive Register 0xF8 (raw pressure LSB = 0x96)
    send >> 00, receive << 00 - Receive Register 0xF9 (raw pressue XLSB = 0x00)
    send >> 00, receive << 83 - Receive Register 0xFA (raw temperature MSB = 0x83)
    send >> 00, receive << a1 - Receive Register 0xFB (raw temperature LSB = 0xA1)
    send >> 00, receive << 00 - Receive Register 0xFC (raw temperature XLSB = 0x00)
    send >> 00, receive << 7f - Receive Register 0xFD (raw humidity MSB = 0x7F)
    send >> 00, receive << a6 - Receive Register 0xFE (raw humidity LSB = 0xA6) */

//  Number of SPI bytes to send and receive.
#define TX_LEN 15
#define RX_LEN 15

//  SPI bytes to be sent.  These bytes will update and read BME280 registers.
static uint8_t tx_buf[TX_LEN] = {  //  Must be in static memory not stack memory because it will be used for DMA.
  0x72,  //  Send command to write to BME280 Register 0xF2 “ctrl_hum”
  0x01,  //  Set the humidity data acquisition options to 0x01
  0x74,  //  Send command to write to BME280 Register 0xF4 “ctrl_meas”
  0x25,  //  Set the pressure and temperature data acquisition options to 0x25
  0x75,  //  Send command to write to BME280 Register 0xF5 “config”
  0xa0,  //  Set the rate, filter and interface options to 0xA0
  0xf7,  //  Send command to burst read from BME280 Registers 0xF7 to 0xFE (“press”, “temp”, “hum”)
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  //  Receive Registers 0xF7 to 0xFE
};

//  Buffer for receiving SPI bytes from BME280.
static uint8_t rx_buf[RX_LEN];  //  Must be in static memory not stack memory because it will be used for DMA.

//  TODO: BME280 temperature compensation parameters.  For simplicity, we hardcode here.  Instead, we should read
//  from BME280 registers 0x89 to 8x8D (dig_T1, dig_T2, dig_T3).
static uint8_t m_dig[] = { 0x97, 0x6e, 0xe6, 0x65, 0x32, 0x00 };

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

static SensorContext sensorContext;       //  Remembers the sensor context.
static float sensorData[sensorDataSize];  //  Array of floats for remembering the sensor data.

static void init_sensor(void) {
  //  Initialise the sensor port. Assume sensor and sensorContext objects have been populated.
  sensor.port_id = BME280_EVENT_SENSOR_PORT;  //  Connect to the sensor at this port.
  sensor.port = spi_setup(sensor.port_id);    //  Get the SPI port.
  if (sensor.port == NULL) { return; }        //  Quit if the setup failed.

  //  Configure the SPI port, specific to the BME280 sensor.  Change this for other sensors.
  spi_configure(
    sensor.port,  //  SPI port to be configured.
    500 * kHz,    //  Maximum speed supported by the sensor.
    MSBFIRST,     //  Bit order: MSBFIRST or LSBFIRST.
    SPI_MODE0);   //  Data mode: SPI_MODE0, 1, 2 or 3.
}

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
