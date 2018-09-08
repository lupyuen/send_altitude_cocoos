//  Sample application demonstrating multitasking of multiple IoT sensors and
//  network transmission on discoveryF4 with cocoOS.

//  Based on https://github.com/lupyuen/send_altitude_cocoos

#include <cocoos.h>
#include "config.h"
#include "sensor.h"
#include "aggregate.h"
#include "network.h"
#include "gps_sensor.h"
#include "wisolRadio.h"
#include "uartSerial.h"
#include "stm32setup.h"



static void system_setup(void);
static void sensor_setup(uint8_t sensor_aggregator_task_id);


// Global semaphore for preventing concurrent access to the single shared I2C Bus
Sem_t i2cSemaphore;


static UartSerial::ptr createDebugConsole() {
  static UartSerial console(DEBUG_USART_ID);
  return &console;
}

static UartSerial::ptr createRadioUartConnection() {
  static UartSerial radioUart(WISOL_USART_ID);
  return &radioUart;
}


static void system_setup(void) {

  stm32_setup();

  os_disable_interrupts();

  // Create the global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  i2cSemaphore = sem_counting_create( maxCount, initValue );
}

static void sensor_setup(uint8_t sensor_aggregator_task_id) {
  //  Edit this function to add your own sensors.

  // get the sensor context for each sensor
  SensorContext *gpsContext  = gps_setup(0, sensor_aggregator_task_id);

  // and create one task for each sensor, passing the context
  task_create(sensor_task, gpsContext, 100,0, 0, 0);
}


int main(void) {
  /************************/
  /*  Hardware Setup      */
  /************************/
  system_setup();


  /************************/
  /*  cocoOS Setup        */
  /************************/
  os_init();


  /************************/
  /*  Network Setup      */
  /************************/
  static WisolRadio radio(createRadioUartConnection());
  uint8_t network_task_id = network_setup(&radio);


  /************************/
  /*  Aggregator Setup    */
  /************************/
  uint8_t sensor_aggregator_id = aggregate_setup(network_task_id);
  

  /************************/
  /*  Sensor Setup      */
  /************************/
  sensor_setup(sensor_aggregator_id);



  os_start();  //  Never returns.  

  return EXIT_SUCCESS;
}




