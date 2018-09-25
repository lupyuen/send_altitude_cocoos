//  Sample application demonstrating multitasking of multiple IoT sensors and network transmission on Arduino with cocoOS.
//  Based on https://github.com/lupyuen/cocoOSExample-arduino
//  Note: Never use the "new" operator like:
//    serialPort = new SoftwareSerial(rx, tx);
//  On STM32 this pulls in the C++ Demangler (cp-demangle.o) which adds 27 KB of useless code.
//  https://docs.google.com/spreadsheets/d/1g_6hTMSofzywcjiIgl0YKy3Qodn9TXdNRaKW-Qyn5s0/edit#gid=517105377
#include "platform.h"
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"            //  For Display Task and debug functions
#include "sensor.h"             //  For common sensor declarations
#include "aggregate.h"          //  For aggregating sensor data before transmission
#include "temp_sensor.h"        //  Temperature sensor (BME280 I2C or SPI)
#include "humid_sensor.h"       //  Humidity sensor (BME280 I2C or SPI)
#include "alt_sensor.h"         //  Altitude sensor (BME280 I2C or SPI)
#include "temp_event_sensor.h"  //  Temperature event-based sensor (BME280 SPI)
#ifdef USE_GYRO_SENSOR          //  If we are using simulated gyro sensor...
#include "gyro_sensor.h"        //  Gyroscope sensor (simulated)
#endif  //  USE_GYRO_SENSOR

//  These are the functions that we will implement in this file.
static void system_setup(void);                       //  Initialise the system.
static void sensor_setup(uint8_t display_task_id);    //  Start the sensor tasks for each sensor to read and process sensor data.

#ifdef TRANSMIT_SENSOR_DATA          //  If we are transmitting sensor data to the IoT network...
#include "uart.h"   //  For UART Task
#include "wisol.h"  //  For Network Task
static uint8_t network_setup(void);  //  Start the network task to send and receive network messages.
static char uartResponse[MAX_UART_RESPONSE_MSG_SIZE + 1];  //  Buffer for writing UART response.
static UARTContext uartContext;      //  For UART Task context.
static NetworkContext wisolContext;  //  For Network Task context.
static UARTMsg uartMsgPool[UART_MSG_POOL_SIZE];  //  Pool of UART messages for the UART Tasks message queue.
static SensorMsg networkMsgPool[NETWORK_MSG_POOL_SIZE];  //  Pool of sensor data messages for the Network Task message queue.
#endif  //  TRANSMIT_SENSOR_DATA

#ifdef SENSOR_DISPLAY                //  If we are displaying sensor data instead of sending to network...
static uint8_t display_setup(void);  //  Start the display task that displays sensor data.  Return the task ID.
static DisplayMsg displayMsgPool[DISPLAY_MSG_POOL_SIZE];  //  Pool of display messages that make up the display message queue.
#endif  //  SENSOR_DISPLAY

int main(void) {
  //  The application starts here. We create the tasks to read and send sensor data 
  //  and start the task scheduler. Note: Arduino's setup() and loop() will not be called since main() is defined.

  enable_debug();       //  Uncomment to allow display of debug messages in development devices. NOTE: This will hang if no debugger is attached.
  //  disable_debug();  //  Uncomment to disable display of debug messages.  For use in production devices.

  //  Init the platform, cocoOS and create any system objects.
  platform_setup();  //  Arduino or STM32 platform setup.
  os_init();         //  Init cocoOS before creating any multitasking objects.
  system_setup();    //  Setup the system.

  //  Erase the aggregated sensor data.
  setup_aggregate();    //  We will aggregate the sensor data in Network Task before transmitting to network.
  uint8_t task_id = 0;  //  Task ID (for Network Task or Display Task) that will receive sensor data messages.

#ifdef SENSOR_DISPLAY         //  If we are displaying the sensor data instead of sending to network...
  task_id = display_setup();  //  Start the display task that displays sensor data.
#endif  //  SENSOR_DISPLAY

#ifdef TRANSMIT_SENSOR_DATA   //  If we are transmitting sensor data to the IoT network...  
  task_id = network_setup();  //  Start the Network Task and UART Task to send and receive network messages.
#endif  //  TRANSMIT_SENSOR_DATA
  
  //  Start the Sensor Task for each sensor to read sensor data and send to the Network Task or Display Task.
  sensor_setup(task_id);

  //  Start the Arduino or STM32 timer to generate ticks for cocoOS to switch tasks.
  platform_start_timer(os_tick);

  //  Start cocoOS task scheduler, which runs the Sensor Tasks, Network Task and UART Task.
  os_start();  //  Never returns.  
	return EXIT_SUCCESS;
}

#ifdef SENSOR_DATA  //  If we are getting data from sensors and not using hardcoded data...
static void sensor_setup(uint8_t task_id) {
  //  Start the sensor tasks for each sensor to read and process sensor data.
  //  Sensor data will be sent to the message queue at the given task ID,
  //  which is the Network Task or Display Task.  Edit this function to add your own sensors.
  const int pollInterval = 10000;  //  Poll the sensor every 10 seconds.

  //  Set up the sensors and get their sensor contexts.  For each sensor, we will create sensor tasks 
  //  using the same task function sensor_task(), but with unique sensor context.
  //  All task priorities must be unique, from 1 (highest priority) to 254 (lowest).
  //  task_create(..., 0, 0, 0) means that the task will not receive any message queue data.  //  debug(F("task_create"));

#if defined(STM32) && defined(USE_TEMP_EVENT_SENSOR)
  //  Use the new Event-based Sensor that's better for multitasking.
  SensorContext *tempEventContext = setup_temp_event_sensor(pollInterval, task_id);
  task_create(sensor_task, tempEventContext, 100, 0, 0, 0);  //  Priority 100 = lower priority than network task    
#endif  //  STM32 && USE_TEMP_EVENT_SENSOR

#ifdef USE_TEMP_POLLING_SENSOR
  //  Use the older Polling-based Sensor that's not so good for multitasking.
  SensorContext *tempContext = setup_temp_sensor(pollInterval, task_id);
  task_create(sensor_task, tempContext, 110, 0, 0, 0);   //  Priority 110
#endif  //  USE_TEMP_POLLING_SENSOR

#ifdef USE_HUMIDITY_SENSOR  //  Use real or simulated humidity sensor.
  SensorContext *humidContext = setup_humid_sensor(pollInterval, task_id);
  task_create(sensor_task, humidContext, 120, 0, 0, 0);  //  Priority 120
#endif  //  USE_HUMIDITY_SENSOR

#ifdef USE_ALTITUDE_SENSOR  //  Use real or simulated altitude sensor.
  SensorContext *altContext = setup_alt_sensor(pollInterval, task_id);
  task_create(sensor_task, altContext, 130, 0, 0, 0);   //  Priority 130
#endif  //  USE_ALTITUDE_SENSOR

#ifdef USE_GYRO_SENSOR      //  Use simulated gyro sensor.
  SensorContext *gyroContext = setup_gyro_sensor(pollInterval, task_id);
  task_create(sensor_task, gyroContext, 140, 0, 0, 0);  //  Priority 140
#endif  //  USE_GYRO_SENSOR

}
#endif  //  SENSOR_DATA

#ifdef TRANSMIT_SENSOR_DATA  //  If we are transmitting sensor data to the IoT network...
static uint8_t network_setup(void) {
  //  Start the Network Task to send and receive network messages.
  //  Also starts the UART Task called by the Network Task to send/receive data to the UART port.
  //  UART Task must have the highest task priority because it must respond to UART data immediately.

  //  Start the UART Task for transmitting UART data to the Wisol module.
  setup_uart(
    &uartContext,  //  Init the context for UART Task.
    uartResponse); //  UART Task will save the response data here.
  uint8_t uartTaskID = task_create(
    uart_task,     //  Task will run this function.
    &uartContext,  //  task_get_data() will be set to the display object.
    10,            //  Priority 10 = highest priority
    (Msg_t *) uartMsgPool,  //  Pool to be used for storing the queue of UART messages.
    UART_MSG_POOL_SIZE,     //  Size of queue pool.
    sizeof(UARTMsg));       //  Size of queue message.

  //  Start the Network Task for receiving sensor data and transmitting to UART Task.
  setup_wisol(
    &wisolContext,  //  Init the context for the Network Task.
    &uartContext,
    uartTaskID, 
    COUNTRY_SG,  //  Change this to your country code. Affects the Sigfox frequency used.
    false);      //  Must be false because we are not using the Sigfox emulator.
  uint8_t networkTaskID = task_create(
      network_task,   //  Task will run this function.
      &wisolContext,  //  task_get_data() will be set to the display object.
      20,             //  Priority 20 = lower priority than UART task
      (Msg_t *) networkMsgPool,  //  Pool to be used for storing the queue of UART messages.
      NETWORK_MSG_POOL_SIZE,     //  Size of queue pool.
      sizeof(SensorMsg));   //  Size of queue message.
  return networkTaskID;
}
#endif  //  TRANSMIT_SENSOR_DATA

static void system_setup(void) {
  //  Initialise the system.
#ifdef SENSOR_DISPLAY  //  If we are displaying the sensor data instead of sending to network...
  init_display();  //  Setup the display objects.
#endif  //  SENSOR_DISPLAY
}

#ifdef SENSOR_DISPLAY  //  If we are displaying the sensor data instead of sending to network...
static uint8_t display_setup(void) {
  //  Start the display task that displays sensor data.  Return the task ID.
  uint8_t display_task_id = task_create(
    display_task,   //  Task will run this function.
    get_display(),  //  task_get_data() will be set to the display object.
    200,            //  Priority 200 = lowest priority
    (Msg_t *) displayMsgPool,  //  Pool to be used for storing the queue of display messages.
    DISPLAY_MSG_POOL_SIZE,        //  Size of queue pool.
    sizeof(DisplayMsg));       //  Size of queue message.
  return display_task_id;
}
#endif  //  SENSOR_DISPLAY

//  Disable exceptions for abstract classes. From https://arobenko.gitbooks.io/bare_metal_cpp/content/compiler_output/abstract_classes.html
extern "C" void __cxa_pure_virtual() { while (true) {} }
void operator delete(void *) { }

//  Arduino-specific functions are in send_altitude_cocoos.ino.
//  STM32-specific functions are in stm32/bluepill library.
