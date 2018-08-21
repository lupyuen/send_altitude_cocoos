//  Sample application demonstrating multitasking of multiple IoT sensors and
//  network transmission on Arduino with cocoOS.
//  Based on https://github.com/lupyuen/cocoOSExample-arduino
#include "platform.h"
// #include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "uart.h"
#include "wisol.h"
#include "sensor.h"
#include "aggregate.h"
#include "temp_sensor.h"   //  Temperature sensor (BME280)
#include "humid_sensor.h"  //  Humidity sensor (BME280)
#include "alt_sensor.h"    //  Altitude sensor (BME280)
#ifdef GYRO_SENSOR
#include "gyro_sensor.h"   //  Gyroscope sensor (simulated)
#endif

//  These are the functions that we will implement in this file.
static void system_setup(void);  //  Initialise the system.
static void sensor_setup(uint8_t display_task_id);    //  Start the sensor tasks for each sensor to read and process sensor data.
static uint8_t network_setup(void);  //  Start the network task to send and receive network messages.
#if defined(ARDUINO)
static void arduino_setup(void);  //  Initialise the Arduino timers.
static void arduino_start_timer(void);  //  Start the AVR Timer 1 to generate interrupt ticks for cocoOS to perform task switching.
#elif defined(STM32)
static void stm32_setup(void);  //  Initialise the STM32 platform.
static void stm32_start_timer(void);  //  Start the STM32 Timer to generate interrupt ticks for cocoOS to perform task switching.
#endif
#ifdef SENSOR_DISPLAY
static uint8_t display_setup(void);  //  Start the display task that displays sensor data.  Return the task ID.
#endif  //  SENSOR_DISPLAY

Sem_t i2cSemaphore;  //  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
static char uartResponse[MAX_UART_RESPONSE_MSG_SIZE + 1];  //  Buffer for writing UART response.
static UARTContext uartContext;
static NetworkContext wisolContext;
static UARTMsg uartMsgPool[UART_MSG_POOL_SIZE];  //  Pool of UART messages for the UART Tasks message queue.
static SensorMsg networkMsgPool[NETWORK_MSG_POOL_SIZE];  //  Pool of sensor data messages for the Network Task message queue.
#ifdef SENSOR_DISPLAY
static DisplayMsg displayMsgPool[DISPLAY_MSG_POOL_SIZE];  //  Pool of display messages that make up the display message queue.
#endif  //  SENSOR_DISPLAY

int main(void) {
  //  The application starts here. We create the tasks to read and display sensor data 
  //  and start the task scheduler. Note: setup() and loop() will not be called since main() is defined.

  //  Init the system and OS for cocoOS.
  system_setup();
  os_init();

  //  Erase the aggregated sensor data.
  setup_aggregate();

#ifdef SENSOR_DISPLAY
  //  Start the display task that displays sensor data.
  uint8_t task_id = display_setup();
#endif  //  SENSOR_DISPLAY

  //  Start the network task to send and receive network messages.
  uint8_t task_id = network_setup();
  
  //  Start the sensor tasks for each sensor to read sensor data and send
  //  to the Network Task or Display Task.
  sensor_setup(task_id);

#if defined(ARDUINO)
  arduino_start_timer();  //  Start the Arduino AVR timer to generate ticks for cocoOS to switch tasks.
#elif defined(STM32)
  stm32_start_timer();  //  TODO: Start the STM32 timer to generate ticks for cocoOS to switch tasks.
#endif

  //  Start cocoOS task scheduler, which runs the sensor tasks and display task.
  os_start();  //  Never returns.  
	return EXIT_SUCCESS;
}

static uint8_t network_setup(void) {
  //  Start the network task to send and receive network messages.

  //  Start the UART Task for transmitting UART data to the Wisol module.
  const uint8_t WISOL_TX = 4;  //  Transmit port for Wisol module.
  const uint8_t WISOL_RX = 5;  //  Receive port for Wisol module.
  setup_uart(
    &uartContext,
    uartResponse,
    WISOL_RX, 
    WISOL_TX, 
    true);
  uint8_t uartTaskID = task_create(
    uart_task,     //  Task will run this function.
    &uartContext,  //  task_get_data() will be set to the display object.
    10,            //  Priority 10 = highest priority
    (Msg_t *) uartMsgPool,  //  Pool to be used for storing the queue of UART messages.
    UART_MSG_POOL_SIZE,     //  Size of queue pool.
    sizeof(UARTMsg));       //  Size of queue message.

  //  Start the Network Task for receiving sensor data and transmitting to UART Task.
  setup_wisol(
    &wisolContext,
    &uartContext,
    uartTaskID, 
    COUNTRY_SG, 
    false);
  uint8_t networkTaskID = task_create(
      network_task,   //  Task will run this function.
      &wisolContext,  //  task_get_data() will be set to the display object.
      20,             //  Priority 20 = lower priority than UART task
      (Msg_t *) networkMsgPool,  //  Pool to be used for storing the queue of UART messages.
      NETWORK_MSG_POOL_SIZE,     //  Size of queue pool.
      sizeof(SensorMsg));   //  Size of queue message.
    
  return networkTaskID;
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

static void stm32_setup() {
}

static void stm32_start_timer() {
}

static void system_setup(void) {
  //  Initialise the system. Create the semaphore.
#if defined(ARDUINO)
  arduino_setup(); 
#elif defined(STM32)
  stm32_setup();  //  TODO
#endif

#ifdef SENSOR_DISPLAY    //// debug(F("init_display")); ////
  init_display();
#endif  //  SENSOR_DISPLAY

  //  Create the global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
  debug(F("Create semaphore")); ////
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  i2cSemaphore = sem_counting_create( maxCount, initValue );
}

#ifdef SENSOR_DISPLAY
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

volatile uint32_t tickCount = 0;  //  Number of millisecond ticks elapsed.

#ifdef ARDUINO
static void arduino_setup(void) {
  //  Initialise the Arduino timers, since we are using main() instead of setup()+loop().
  init();
  debug(F("----arduino_setup"));
}

static void arduino_start_timer(void) {
  //  Start the AVR Timer 1 to generate interrupt ticks every millisecond
  //  for cocoOS to perform task switching.  AVR Timer 0 is reserved for 
  //  Arduino timekeeping. From https://arduinodiy.wordpress.com/2012/02/28/timer-interrupts/
  cli();          //  Disable global interrupts
  TCCR1A = 0;     //  Set entire TCCR1A register to 0
  TCCR1B = 0;     //  Same for TCCR1B 
  OCR1A = 16000;    //  Set compare match register to desired timer count  
  TCCR1B |= (1 << WGM12);  //  Turn on CTC mode (Clear Timer on Compare Match)
  TCCR1B |= (1 << CS10);   //  Set timer prescaler as 1 (fastest)
  // TCCR1B |= (1 << CS11);   //  Set timer prescaler as 8 (fast)
  TIMSK1 |= (1 << OCIE1A);  //  Enable timer compare interrupt
  sei();          //  Enable global interrupts
}

ISR(TIMER1_COMPA_vect) {
  //  Handle the AVR Timer 1 interrupt. Trigger an os_tick() for cocoOS to perform task switching.
  ////  debug(F("os_tick")); ////
  tickCount++;
  os_tick();
}
#endif  //  ARDUINO
