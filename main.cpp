//  Sample application demonstrating multitasking of multiple IoT sensors and
//  network transmission on discoveryF4 with cocoOS.

//  Based on https://github.com/lupyuen/send_altitude_cocoos

#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "sensor.h"
#include "aggregate.h"
#include "gps_sensor.h"
#include "stm32setup.h"
#include "config.h"
#include "network.h"
#include "wisolRadio.h"
#include "uartSerial.h"



static void system_setup(void);
static uint8_t network_setup(void);
static uint8_t sensor_aggregator_setup(uint8_t network_task);
static void sensor_setup(uint8_t sensor_aggregator_task_id);

static UartSerial::ptr createDebugConsole();
static UartSerial::ptr createRadioUartConnection();


// Global semaphore for preventing concurrent access to the single shared I2C Bus
Sem_t i2cSemaphore;

// Buffer for writing radio response.
static char radioResponse[MAX_RADIO_RESPONSE_MSG_SIZE + 1];

// Task contexts
static AggregateContext aggregateContext;
static NetworkContext networkContext;

// Pool of network messages for the network tasks message queue.
static NetworkMsg networkMsgPool[RADIO_MSG_POOL_SIZE];

// Pool of sensor data messages for the aggregator task message queue.
static SensorMsg sensorMsgPool[NETWORK_MSG_POOL_SIZE];


int main(void) {
  //  The application starts here. We create the tasks to read and display sensor data 
  //  and start the task scheduler. Note: setup() and loop() will not be called since main() is defined.

  //  Init the system and OS for cocoOS.
  system_setup();
  os_init();

  //  Start the radio task to send and receive network messages.
  uint8_t network_task_id = network_setup();

  // the sensor aggregator needs the radio task id
  uint8_t sensor_aggregator_id = sensor_aggregator_setup(network_task_id);
  
  // and the sensor tasks needs the aggregator id
  sensor_setup(sensor_aggregator_id);

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

static UartSerial::ptr createRadioUartConnection() {
  static UartSerial radioUart(WISOL_USART_ID);
  return &radioUart;
}


static uint8_t network_setup(void) {

  // The radio device connected to the network task
  static WisolRadio radio(createRadioUartConnection());

  networkContext.response = radioResponse;
  networkContext.radio = &radio;
  networkContext.initialized = false;

  uint8_t networkTaskID = task_create(
    network_task,     //  Task will run this function.
    &networkContext,  //  task_get_data() will be set to the display object.
    10,            //  Priority 10 = highest priority
    (Msg_t *) networkMsgPool,  //  Pool to be used for storing the queue of UART messages.
    RADIO_MSG_POOL_SIZE,     //  Size of queue pool.
    sizeof(NetworkMsg));       //  Size of queue message.

  return networkTaskID;
}

static uint8_t sensor_aggregator_setup(uint8_t network_task) {
  //  Start the Aggregate Task for receiving sensor data and transmitting to radio Task.
  aggregateContext.networkTaskID = network_task;
  aggregateContext.sendPeriodInSeconds = 30;
  setup_aggregate();

  uint8_t aggregateTaskId = task_create(
      aggregate_task,   //  Task will run this function.
      &aggregateContext,  //  task_get_data() will be set to the display object.
      20,             //  Priority 20 = lower priority than UART task
      (Msg_t *) sensorMsgPool,  //  Pool to be used for storing the queue of UART messages.
      NETWORK_MSG_POOL_SIZE,     //  Size of queue pool.
      sizeof(SensorMsg));   //  Size of queue message.
    
  return aggregateTaskId;
}

static void sensor_setup(uint8_t sensor_aggregator_task_id) {
  //  Edit this function to add your own sensors.

  //  Set up the sensors and get their sensor contexts.
  const int pollInterval = 5000;  //  Poll the sensor every 5000 milliseconds.

  // get the sensor context for each sensor
  SensorContext *gpsContext  = setup_gps_sensor(0, sensor_aggregator_task_id);

  // and create one task for each sensor, passing the context
  task_create(sensor_task, gpsContext, 100,0, 0, 0);
}


