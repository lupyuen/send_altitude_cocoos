/**
 * Example application demonstrating handling of multiple IoT sensors.
 * Derived from https://github.com/lupyuen/cocoOSExample-arduino
 *
 * The application consists of 3 tasks:
 *
 *  - temp sensor task: waits for an event to signaled from the temp sensor driver. New temp
 *    data is fetched from the sensor and sent in a message to the display task.
 *
 *  - gyro sensor task: waits for a timeout and then polls the sensor for new data. New data is
 *    fetched from the sensor and sent in a message to the display task.
 *
 *  - display task: writes sensor values to the terminal
 *
 *  Main flow of execution:
 *  When the sensors are serviced they eventually signals new data available by signaling an event (temp
 *  sensor) or sets a polled flag (gyro sensor). The sensor tasks waiting for event/timeout, checks the
 *  sensor for new data and posts a message with the new data to the display task.
 *
 *  Task procedure sharing
 *  The two sensor tasks use the same task procedure. All work and data handling is done through the task
 *  data pointer assigned to each task. This points to a structure holding sensor configuration/functions
 *  and an array holding sensor data.
 *
 */
//  Arduino declarations
#include <Arduino.h> ////
#include <Time.h> ////
#include <TimeLib.h> ////
typedef unsigned long time_t; ////  TODO: Fix the declaration

//  Other declarations
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <cocoos-cpp.h>  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"
#include "temp_sensor.h"
#include "gyro_sensor.h"
#include "display.h"

//  Serial Monitor will run at this bitrate.
#define SERIAL_BAUD 9600

//  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
Sem_t i2cSemaphore;  //  Declared in sensor.h

//  Pool of display messages that make up the display message queue.
#define displayMsgPoolSize 6  //  Allow only 6 messages, which means fewer than 6 devices.
static DisplayMsg displayMsgPool[displayMsgPoolSize];

//////////////////////////////////////////////
//  TODO: Move to bme280.cpp
#include <BME280I2C.h>
#include <Wire.h>

//  The global instance of the BME API: https://github.com/finitespace/BME280
BME280I2C bme;    // Default : forced mode, standby time = 1000 ms
                  // Oversampling = pressure ×1, temperature ×1, humidity ×1, filter off,

void bme280_setup(void) {
  //  Set up the BME280 module for reading.
  Wire.begin();
  while(!bme.begin()) {
    Serial.println(F("Could not find BME280 sensor!"));
    delay(1000);
  }
  switch(bme.chipModel()) {
     case BME280::ChipModel_BME280:
       Serial.println(F("Found BME280 sensor! Success."));
       break;
     case BME280::ChipModel_BMP280:
       Serial.println(F("Found BMP280 sensor! No Humidity available."));
       break;
     default:
       Serial.println(F("Found UNKNOWN sensor! Error!"));
  }
}

/********************************************/
/*            System threads                */
/********************************************/

static void arduino_start_timer(void) { ////
  //  Start the AVR Timer 1 to generate interrupt ticks for cocoOS to perform
  //  background processing.  AVR Timer 0 is reserved for Arduino timekeeping.
  
  // Set PORTB pins as output, but off
	DDRB = 0xFF;
	PORTB = 0x00;

	// Turn on timer 
	// TCCR1B |= _BV(CS10);  // no prescaler
	TCCR1B = (1<<CS10) | (1<<CS12); //set the prescaler as 1024
	TIMSK1 |= _BV(TOIE1);

	// Turn interrupts on.
	sei();	
} ////

ISR(TIMER1_OVF_vect) { ////
  //  Handle the AVR Timer 1 interrupt. Trigger an os_tick() for cocoOS to perform background processing.
  // debug("os_tick"); ////
  os_tick();  
} ////

/********************************************/
/*            Setup and main                */
/********************************************/

static void arduino_setup(void) { ////
  //  Run initialisation for Arduino, since we are using main() instead of setup()+loop().
  init();  // Initialize Arduino timers,
  debug("----arduino_setup", 0);
} ////

static void system_setup(void) {
  //  Run system initialisation.
  arduino_setup(); ////
  //// debug("init_display", 0); ////
  init_display();

  //  Create the global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
  debug("Create semaphore", 0); ////
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  i2cSemaphore = sem_counting_create( maxCount, initValue );
}

static void sensor_setup(uint8_t display_task_id) {
  //  Start the sensor tasks for each sensor to read and process sensor data.

  //  Set up the BME280 API.
  bme280_setup();

  //  Set up the sensors and get the sensor contexts.
  const int pollIntervalMillisec = 500;  //  Poll the sensor every 500 milliseconds.
  SensorContext *tempContext = setup_temp_sensor(TEMP_DATA, pollIntervalMillisec, display_task_id);
  SensorContext *gyroContext = setup_gyro_sensor(GYRO_DATA, pollIntervalMillisec, display_task_id);

  //  For each sensor, create sensor tasks using same task function, but with unique sensor context.
  //  "0, 0, 0" means that the tasks may not receive any message queue data.
  //// debug("task_create"); ////
  task_create(sensor_task, tempContext, 10,  //  Priority 10 = highest priority
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensor_task, gyroContext, 20,  //  Priority 20
    0, 0, 0);  //  Will not receive message queue data.
}

int main(void) {
  //  This is the entry point for the application. We create the tasks and start the task scheduler.
  //  Note: setup() and loop() will not be called since main() is defined.

  //  Init the system and OS for cocoOS.
  system_setup();
  os_init();

  //  Start the display task that displays sensor readings
  uint8_t display_task_id = task_create(
    display_task,  //  Task will run this function.
    get_display(),  //  task_get_data() will be set to the display object.
    100,  //  Priority 100 = lowest priority
    (Msg_t *) displayMsgPool,  //  Pool to be used for storing the queue of display messages.
    displayMsgPoolSize,  //  Size of queue pool.
    sizeof(DisplayMsg));  //  Size of queue message.
  
  //  Start the sensor tasks for each sensor to read and process sensor data.
  sensor_setup(display_task_id);

  //  Start the AVR timer to generate ticks for background processing.
  //// debug("arduino_start_timer"); ////
  arduino_start_timer(); ////

  //  Start cocoOS task scheduler, which runs the sensor tasks and display task.
  //// debug("os_start"); ////
  os_start();  //  Never returns.
  
	return EXIT_SUCCESS;
}

void debug(const char *s1, const char *s2 = 0) {
  //  Print a message to the Arduino serial console. This code is located here because 
  //  Serial API may only be used in a C++ module.  Declared in sensor.h
  Serial.begin(SERIAL_BAUD);
  while(!Serial) {}  //  Wait for Serial to be ready.

  Serial.print(s1);
  if (s2) Serial.print(s2);
  Serial.println("");
  Serial.flush();  //  Let serial printing finish.
}
