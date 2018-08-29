//  Sample application demonstrating multitasking of multiple IoT sensors and
//  network transmission on discoveryF4 with cocoOS.

//  Based on https://github.com/lupyuen/cocoOSExample-arduino

#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "wisol.h"
#include "sensor.h"
#include "aggregate.h"
#include "temp_sensor.h"   //  Temperature sensor (BME280)
#include "humid_sensor.h"  //  Humidity sensor (BME280)
#include "alt_sensor.h"    //  Altitude sensor (BME280)
#include "stm32setup.h"
#include "config.h"
#include "radio.h"
#include "radios/wisolRadio.h"
#include "uartSerial.h"
#ifdef GYRO_SENSOR
#include "gyro_sensor.h"   //  Gyroscope sensor (simulated)
#endif

static void system_setup(void);
static void sensor_setup(uint8_t display_task_id);
static uint8_t network_setup(void);
static UartSerial::ptr createDebugConsole();


// Global semaphore for preventing concurrent access to the single shared I2C Bus
Sem_t i2cSemaphore;

// Buffer for writing radio response.
static char radioResponse[MAX_RADIO_RESPONSE_MSG_SIZE + 1];

// Task contexts
static AggregateContext aggregateContext;
static RadioContext radioContext;
//static NetworkContext networkContext;

// Pool of UART messages for the UART Tasks message queue.
static RadioMsg radioMsgPool[RADIO_MSG_POOL_SIZE];

// Pool of sensor data messages for the Network Task message queue.
static SensorMsg networkMsgPool[NETWORK_MSG_POOL_SIZE];


int main(void) {
  //  The application starts here. We create the tasks to read and display sensor data 
  //  and start the task scheduler. Note: setup() and loop() will not be called since main() is defined.

  //  Init the system and OS for cocoOS.
  system_setup();
  os_init();

  //  Erase the aggregated sensor data.
  //setup_aggregate();

  //  Start the network task to send and receive network messages.
  uint8_t task_id = network_setup();
  
  //  Start the sensor tasks for each sensor to read sensor data and send
  //  to the Network Task or Display Task.
  sensor_setup(task_id);


  //  Start cocoOS task scheduler, which runs the sensor tasks and display task.
  os_start();  //  Never returns.  

  return EXIT_SUCCESS;
}

static void system_setup(void) {
  //  Initialise the system. Create the semaphore.

  stm32_setup();
  os_disable_interrupts();

  // Create the global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  i2cSemaphore = sem_counting_create( maxCount, initValue );
}

static UartSerial::ptr createDebugConsole() {
  static UartSerial console(DEBUG_USART_ID);
  return &console;
}

static uint8_t network_setup(void) {
  //  Start the network task to send and receive network messages.
  //  Start the Radio Task for transmitting data to the Wisol module.

  static WisolRadio radio(createDebugConsole());

  radioContext.response = radioResponse;
  radioContext.radio = &radio;
  radioContext.initialized = false;

  uint8_t radioTaskID = task_create(
    radio_task,     //  Task will run this function.
    &radioContext,  //  task_get_data() will be set to the display object.
    10,            //  Priority 10 = highest priority
    (Msg_t *) radioMsgPool,  //  Pool to be used for storing the queue of UART messages.
    RADIO_MSG_POOL_SIZE,     //  Size of queue pool.
    sizeof(RadioMsg));       //  Size of queue message.

  //  Start the Aggregate Task for receiving sensor data and transmitting to radio Task.
  aggregateContext.radioTaskID = radioTaskID;
  aggregateContext.sendPeriodInSeconds = 900;
  setup_aggregate();

  uint8_t aggregateTaskId = task_create(
      aggregate_task,   //  Task will run this function.
      &aggregateContext,  //  task_get_data() will be set to the display object.
      20,             //  Priority 20 = lower priority than UART task
      (Msg_t *) networkMsgPool,  //  Pool to be used for storing the queue of UART messages.
      NETWORK_MSG_POOL_SIZE,     //  Size of queue pool.
      sizeof(SensorMsg));   //  Size of queue message.
    
  return aggregateTaskId;
}

#ifdef SENSOR_DATA  //  Use real not simulated sensors.
static void sensor_setup(uint8_t task_id) {
  //  Start the sensor tasks for each sensor to read and process sensor data.
  //  Sensor data will be sent to the message queue at the given task ID,
  //  which is the Network Task or Display Task.
  //  Edit this function to add your own sensors.

  //  Set up the sensors and get their sensor contexts.
  const int pollInterval = 5000;  //  Poll the sensor every 5000 milliseconds.
  SensorContext *tempContext = setup_temp_sensor(pollInterval, task_id);
  SensorContext *humidContext = setup_humid_sensor(pollInterval, task_id);
  SensorContext *altContext = setup_alt_sensor(pollInterval, task_id);
#ifdef GYRO_SENSOR
  SensorContext *gyroContext = setup_gyro_sensor(pollInterval, task_id);
#endif  //  GYRO_SENSOR

  //  For each sensor, create sensor tasks using the same task function, but with unique sensor context.
  //  "0, 0, 0" means that the tasks may not receive any message queue data.
  //// debug(F("task_create")); ////
  task_create(sensor_task, tempContext, 100,   //  Priority 100 = lower priority than network task
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensor_task, humidContext, 120,  //  Priority 120
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensor_task, altContext, 130,  //  Priority 130
    0, 0, 0);  //  Will not receive message queue data.
#ifdef GYRO_SENSOR
  task_create(sensor_task, gyroContext, 140,   //  Priority 140
    0, 0, 0);  //  Will not receive message queue data.
#endif  //  GYRO_SENSOR
}
#endif  //  SENSOR_DATA


volatile uint32_t tickCount = 0;  //  Number of millisecond ticks elapsed.

