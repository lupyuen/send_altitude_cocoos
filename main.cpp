//  Sample application demonstrating handling of multiple IoT sensors on Arduino with cocoOS.
//  Based on https://github.com/lupyuen/cocoOSExample-arduino
#include <Arduino.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "display.h"
#include "uart.h"  //  TODO: Remove
#include "wisol.h"
#include "sensor.h"
#include "temp_sensor.h"   //  Temperature sensor (BME280)
#include "humid_sensor.h"  //  Humidity sensor (BME280)
#include "alt_sensor.h"    //  Altitude sensor (BME280)
#include "gyro_sensor.h"   //  Gyroscope sensor (simulated)

//  These are the functions that we will implement in this file.
static void network_setup(uint8_t display_task_id);  //  Start the network task to send and receive network messages.
static void sensor_setup(uint8_t display_task_id);    //  Start the sensor tasks for each sensor to read and process sensor data.
static uint8_t display_setup(void);  //  Start the display task that displays sensor data.  Return the task ID.
static void system_setup(void);  //  Initialise the system.
static void arduino_setup(void);  //  Initialise the Arduino timers.
static void arduino_start_timer(void);  //  Start the AVR Timer 1 to generate interrupt ticks for cocoOS to perform task switching.

Sem_t i2cSemaphore;  //  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
static UARTContext uartContext;
static DisplayMsg displayMsgPool[displayMsgPoolSize];  //  Pool of display messages that make up the display message queue.

int main(void) {
  //  The application starts here. We create the tasks to read and display sensor data 
  //  and start the task scheduler. Note: setup() and loop() will not be called since main() is defined.

  //  Init the system and OS for cocoOS.
  system_setup();
  os_init();

  //  Start the display task that displays sensor data.
  uint8_t display_task_id = display_setup();

  //  Start the network task to send and receive network messages.
  network_setup(display_task_id);
  
  //  Start the sensor tasks for each sensor to read sensor data.
  //// TODO: sensor_setup(display_task_id);

  //  Start the Arduino AVR timer to generate ticks for cocoOS to switch tasks.
  //// debug(F("arduino_start_timer")); ////
  arduino_start_timer(); ////

  //  Start cocoOS task scheduler, which runs the sensor tasks and display task.
  //// debug(F("os_start")); ////
  os_start();  //  Never returns.  
	return EXIT_SUCCESS;
}

static void network_setup(uint8_t display_task_id) {
  //  Start the network task to send and receive network messages.

  //  TODO
  task_create(uart_task, &uartContext, 10,   //  Priority 10 = highest priority
    0, 0, 0);  //  Will not receive message queue data.
}

static void sensor_setup(uint8_t display_task_id) {
  //  Start the sensor tasks for each sensor to read and process sensor data.
  //  Edit this function to add your own sensors.

  //  Set up the sensors and get their sensor contexts.
  const int pollInterval = 500;  //  Poll the sensor every 500 milliseconds.
  SensorContext *tempContext = setup_temp_sensor(pollInterval, display_task_id);
  SensorContext *humidContext = setup_humid_sensor(pollInterval, display_task_id);
  SensorContext *altContext = setup_alt_sensor(pollInterval, display_task_id);
  SensorContext *gyroContext = setup_gyro_sensor(pollInterval, display_task_id);

  //  For each sensor, create sensor tasks using the same task function, but with unique sensor context.
  //  "0, 0, 0" means that the tasks may not receive any message queue data.
  //// debug(F("task_create")); ////
  task_create(sensor_task, tempContext, 20,   //  Priority 20 = lower priority than network task
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensor_task, humidContext, 30,  //  Priority 30
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensor_task, altContext, 40,  //  Priority 40
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensor_task, gyroContext, 50,   //  Priority 50
    0, 0, 0);  //  Will not receive message queue data.
}

static uint8_t display_setup(void) {
  //  Start the display task that displays sensor data.  Return the task ID.
  uint8_t display_task_id = task_create(
    display_task,   //  Task will run this function.
    get_display(),  //  task_get_data() will be set to the display object.
    1000,            //  Priority 1000 = lowest priority
    (Msg_t *) displayMsgPool,  //  Pool to be used for storing the queue of display messages.
    displayMsgPoolSize,        //  Size of queue pool.
    sizeof(DisplayMsg));       //  Size of queue message.
  return display_task_id;
}

static void system_setup(void) {
  //  Initialise the system. Create the semaphore.
  arduino_setup(); //// debug(F("init_display")); ////
  init_display();

  //  Create the global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
  debug(F("Create semaphore")); ////
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  i2cSemaphore = sem_counting_create( maxCount, initValue );
}

static void arduino_setup(void) {
  //  Initialise the Arduino timers, since we are using main() instead of setup()+loop().
  init();
  debug(F("----arduino_setup"));
}

static void arduino_start_timer(void) {
  //  Start the AVR Timer 1 to generate interrupt ticks for cocoOS to perform
  //  task switching.  AVR Timer 0 is reserved for Arduino timekeeping.    
  //  TODO: Check millisecond timing
	DDRB = 0xFF;  //  Set PORTB pins as output, but off.
	PORTB = 0x00;
	//  Turn on timer.
	//  TCCR1B |= _BV(CS10);  //  No prescaler.
	TCCR1B = (1<<CS10) | (1<<CS12); //  Set the prescaler as 1024.
	TIMSK1 |= _BV(TOIE1);	
	sei();	// Turn interrupts on.
}

ISR(TIMER1_OVF_vect) {
  //  Handle the AVR Timer 1 interrupt. Trigger an os_tick() for cocoOS to perform task switching.
  ////  debug(F("os_tick")); ////
  os_tick();  
}
