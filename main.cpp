//  Sample application demonstrating handling of multiple IoT sensors on Arduino with cocoOS.
//  Based on https://github.com/lupyuen/cocoOSExample-arduino
#include <Arduino.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "display.h"
#include "sensor.h"
#include "temp_sensor.h"
#include "gyro_sensor.h"

#define SERIAL_BAUD 9600  //  Serial Monitor will run at this bitrate.
Sem_t i2cSemaphore;  //  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
static DisplayMsg displayMsgPool[displayMsgPoolSize];  //  Pool of display messages that make up the display message queue.

//  These are the functions that we will implement in this file.
static void sensor_setup(uint8_t display_task_id);    //  Start the sensor tasks for each sensor to read and process sensor data.
static uint8_t display_setup(void);  //  Start the display task that displays sensor data.  Return the task ID.
static void system_setup(void);  //  Initialise the system.
static void arduino_setup(void);  //  Initialise the Arduino timers.
static void arduino_start_timer(void);  //  Start the AVR Timer 1 to generate interrupt ticks for cocoOS to perform task switching.

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

int main(void) {
  //  The application starts here. We create the tasks to read and display sensor data 
  //  and start the task scheduler. Note: setup() and loop() will not be called since main() is defined.

  //  Init the system and OS for cocoOS.
  system_setup();
  os_init();

  //  Start the display task that displays sensor data.
  uint8_t display_task_id = display_setup();
  
  //  Start the sensor tasks for each sensor to read sensor data.
  sensor_setup(display_task_id);

  //  Start the Arduino AVR timer to generate ticks for cocoOS to switch tasks.
  //// debug("arduino_start_timer"); ////
  arduino_start_timer(); ////

  //  Start cocoOS task scheduler, which runs the sensor tasks and display task.
  //// debug("os_start"); ////
  os_start();  //  Never returns.  
	return EXIT_SUCCESS;
}

static void sensor_setup(uint8_t display_task_id) {
  //  Start the sensor tasks for each sensor to read and process sensor data.  
  bme280_setup();  //  Set up the BME280 API.

  //  Set up the sensors and get their sensor contexts.
  const int pollInterval = 500;  //  Poll the sensor every 500 milliseconds.
  SensorContext *tempContext = setup_temp_sensor(pollInterval, display_task_id);
  SensorContext *gyroContext = setup_gyro_sensor(pollInterval, display_task_id);

  //  For each sensor, create sensor tasks using the same task function, but with unique sensor context.
  //  "0, 0, 0" means that the tasks may not receive any message queue data.
  //// debug("task_create"); ////
  task_create(sensor_task, tempContext, 10,  //  Priority 10 = highest priority
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensor_task, gyroContext, 20,  //  Priority 20
    0, 0, 0);  //  Will not receive message queue data.
}

static uint8_t display_setup(void) {
  //  Start the display task that displays sensor data.  Return the task ID.
  uint8_t display_task_id = task_create(
    display_task,   //  Task will run this function.
    get_display(),  //  task_get_data() will be set to the display object.
    100,            //  Priority 100 = lowest priority
    (Msg_t *) displayMsgPool,  //  Pool to be used for storing the queue of display messages.
    displayMsgPoolSize,        //  Size of queue pool.
    sizeof(DisplayMsg));       //  Size of queue message.
  return display_task_id;
}

static void system_setup(void) {
  //  Initialise the system. Create the semaphore.
  arduino_setup(); //// debug("init_display"); ////
  init_display();

  //  Create the global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
  debug("Create semaphore", 0); ////
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  i2cSemaphore = sem_counting_create( maxCount, initValue );
}

static void arduino_setup(void) {
  //  Initialise the Arduino timers, since we are using main() instead of setup()+loop().
  init();
  debug("----arduino_setup", 0);
}

static void arduino_start_timer(void) {
  //  Start the AVR Timer 1 to generate interrupt ticks for cocoOS to perform
  //  task switching.  AVR Timer 0 is reserved for Arduino timekeeping.    
	DDRB = 0xFF;  //  Set PORTB pins as output, but off.
	PORTB = 0x00;
	//  Turn on timer.
	//  TCCR1B |= _BV(CS10);  //  No prescaler.
	TCCR1B = (1<<CS10) | (1<<CS12); //  Set the prescaler as 1024.
	TIMSK1 |= _BV(TOIE1);	
	sei();	// Turn interrupts on.
}

ISR(TIMER1_OVF_vect) {
  //  Handle the AVR Timer 1 interrupt. Trigger an os_tick() for cocoOS to perform background processing.
  ////  debug("os_tick"); ////
  os_tick();  
}

void debug(const char *s1, const char *s2) {
  //  Print a message to the Arduino serial console. This code is located here because 
  //  Serial API may only be used in a C++ module.  Declared in sensor.h
  Serial.begin(SERIAL_BAUD);
  while (!Serial) {}  //  Wait for Serial to be ready.
  Serial.print(s1);
  if (s2) Serial.print(s2);
  Serial.println("");
  Serial.flush();  //  Let serial printing finish.
}
