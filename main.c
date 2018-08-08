
/**
 * Example application demonstrating task procedure sharing, message passing and more.
 *
 * The application consists of four tasks:
 *
 *  - temp sensor task: waits for an event to signaled from the temp sensor driver. New temp
 *    data is fetched from the sensor and sent in a message to the display task.
 *
 *  - gyro sensor task: waits for a timeout and then polls the sensor for new data. New data is
 *    fetched from the sensor and sent in a message to the display task.
 *
 *  - control task: waits for up/down arrow events to be signaled and then changes channel of
 *    the temp sensor.
 *
 *  - display task: writes sensor values to the terminal
 *
 *  Two linux threads are created at startup:
 *   - One that simulates a timer tick and calls os_tick(). It also periodically services the
 *     sensors. This makes the sensors signal it has new data by signaling an event or setting
 *     a polled flag.
 *
 *   - One thread that reads characters from stdin and signals cocoOS events when up/down arrow are pressed.
 *
 *  Main flow of execution:
 *  When the sensors are serviced they eventually signals new data available by signaling an event (temp
 *  sensor) or sets a polled flag (gyro sensor). The sensor tasks waiting for event/timeout, checks the
 *  sensor for new data and posts a message with the new data to the display task.
 *
 *  Simultaneously, the thread reading characters from stdin, signals events when up/down
 *  arrow keys are pressed on the keyboard. The control task then changes channel on the temp sensor.
 *
 *  Task procedure sharing
 *  The two sensor tasks use the same task procedure. All work and data handling is done through the task
 *  data pointer assigned to each task. This points to a structure holding sensor configuration/functions
 *  and an array holding sensor data.
 *
 */
#include <Arduino.h> ////
#include <Time.h> ////
#include <TimeLib.h> ////
typedef unsigned long time_t; ////  TODO: Fix the declaration

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <cocoos.h>

#include "sensor.h"
#include "temp_sensor.h"
#include "gyro_sensor.h"
#include "display.h"

//  Global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
Sem_t i2cSemaphore;  //  Declared in sensor.h

static Evt_t tempEvt;
static Evt_t prevChEvt;
static Evt_t nextChEvt;

static uint8_t displayTaskId;

#define sensorDataSize 3  //  Max number of floats to be returned as sensor data.

typedef struct {
  Sensor *sensor;
  float data[sensorDataSize];  //  Array of float sensor data values returned by the sensor.
  uint8_t count;  //  Number of float sensor data values returned by the sensor.
} TaskData;

static TaskData tempTaskData;
static TaskData gyroTaskData;

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
/*            Application tasks             */
/********************************************/

static void sensorTask() {
  //  Run background tasks to receive and process sensor data.
  //  This task will be reused by all sensors: temperature, humidity, altitude.
  //  Don't declare any static variables inside here because they will conflict
  //  with other sensors.
  TaskData *taskData = NULL;  //  Declared outside the task to prevent cross-initialisation error in C++.
  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) {  //  Run the sensor processing code forever. So the task never ends.
    //  We should not make this variable static, because the task data should be unique for each task.
    taskData = (TaskData *) task_get_data();

    //  This code is executed by multiple sensors. We use a global semaphore to prevent 
    //  concurrent access to the single shared I2C Bus on Arduino Uno.
    debug(taskData->sensor->info.name, " >> Wait for semaphore"); ////
    sem_wait(i2cSemaphore);  //  Wait until no other sensor is using the I2C Bus. Then lock the semaphore.
    debug(taskData->sensor->info.name, " >> Got semaphore"); ////

    //  We have to fetch the data pointer again after the wait.
    taskData = (TaskData *) task_get_data();

    //  Do we have new data?
    if (taskData->sensor->info.poll_sensor_func() > 0) {
      //  If we have new data, copy sensor data to task data.
      uint8_t sensorDataCount = taskData->sensor->info.
        receive_sensor_data_func(taskData->data, sensorDataSize);
      taskData->count = sensorDataCount;  //  Number of float values returned.

      // And put it into a display message. Use the sensor id as message signal.
      // Note: When posting a message, its contents is copied into the message queue.
      DisplayMsg msg;
      msg.super.signal = taskData->sensor->info.id;  //  id is either TEMP_DATA or GYRO_DATA.
      memset(msg.name, 0, sensorNameSize + 1);  //  Zero the name array.
      strncpy(msg.name, taskData->sensor->info.name, sensorNameSize);  //  Set the sensor name e.g. tmp
      msg.count = taskData->count;  //  Number of floats returned as sensor data.
      for (int i = 0; i < msg.count && i < sensorDataSize; i++) {
        msg.data[i] = taskData->data[i];
      }
      
      //  Note for Arduino: msg_post() must be called in a C source file, not C++.
      //  The macro expansion fails in C++ with a cross-initialisation error.
      debug(msg.name, " >> Send message"); ////
      msg_post(displayTaskId, msg);
    }

    //  We are done with the I2C Bus.  Release the semaphore so that another task can fetch the sensor data.
    debug(taskData->sensor->info.name, " >> Release semaphore"); ////
    sem_signal(i2cSemaphore);

    //  Wait a short while before polling the sensor again.
    debug(taskData->sensor->info.name, " >> Wait interval"); ////
    task_wait(taskData->sensor->info.poll_interval);
  }
  debug("task_close", 0); ////
  task_close();  //  End of the task. Should never come here.
}

static void displayTask() {
  //  Background task that receives display messages and displays them.
  static DisplayMsg msg;
  task_open();
  for (;;) {
    //  Wait for an incoming display message containing sensor data.
    //// debug("msg_receive", 0); ////
    msg_receive(os_get_running_tid(), &msg);
    Display *display = (Display *) task_get_data();

    //  Update the sensor data to be displayed.
    //// debug(msg.name, " >> updateData"); ////
    display->update_data_func(msg.super.signal, msg.name, msg.data, msg.count);

    //  Display the sensor data.
    //// debug(msg.name, " >> refresh"); ////
    display->refresh_func();
  }
  task_close();
}

/********************************************/
/*            Setup and main                */
/********************************************/

static void arduino_setup(void) { ////
  //  Run initialisation for Arduino, since we are using main() instead of setup()+loop().
  init();  // initialize Arduino timers  
  debug("----arduino_setup", 0);
} ////

static void system_setup(void) {
  arduino_setup(); ////
  debug("display_init", 0); ////
  display_init();

  //  Create the global semaphore for preventing concurrent access to the single shared I2C Bus on Arduino Uno.
  debug("Create semaphore", 0); ////
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  i2cSemaphore = sem_counting_create( maxCount, initValue );
}

static void sensor_setup(void) {
  // create events
  //// debug("event_create", 0); ////
  tempEvt   = event_create();
  prevChEvt = event_create();
  nextChEvt = event_create();

  //  Initialize the sensors.
  //// debug("get_temp_sensor"); ////
  tempTaskData.sensor = get_temp_sensor();
  //// debug("tempSensor.init"); ////
  const int pollIntervalMillisec = 500;  //  Poll the sensor every 500 milliseconds.
  tempTaskData.sensor->control.init_sensor_func(TEMP_DATA, &tempEvt, pollIntervalMillisec);

  //// debug("gyroSensor_get"); ////
  gyroTaskData.sensor = gyroSensor_get();
  //// debug("gyroSensor.init"); ////
  gyroTaskData.sensor->control.init_sensor_func(GYRO_DATA, 0, pollIntervalMillisec);

  //  Create 2 sensor tasks using same task function, but with unique task data.
  //  "0, 0, 0" means that the tasks may not receive any message queue data.
  //// debug("task_create"); ////
  task_create(sensorTask, &tempTaskData, 10,  //  Priority 10 = highest priority
    0, 0, 0);  //  Will not receive message queue data.
  task_create(sensorTask, &gyroTaskData, 20,  //  Priority 20
    0, 0, 0);  //  Will not receive message queue data.
}

//  Pool of display messages that make up the display message queue.
#define displayMsgPoolSize 5
static DisplayMsg displayMsgPool[displayMsgPoolSize];

int main(void) {
  system_setup();
  //// debug("os_init"); ////
  os_init();

  //  Setup the sensors for reading.
  sensor_setup();

  //  Start the display task that displays sensor readings
  displayTaskId = task_create(
    displayTask,  //  Task will run this function.
    display_get(),  //  task_get_data() will be set to the display object.
    100,  //  Priority 100 = lowest priority
    (Msg_t *) displayMsgPool,  //  Pool to be used for storing the queue of display messages.
    displayMsgPoolSize,  //  Size of queue pool.
    sizeof(DisplayMsg));  //  Size of queue message.
  
  //  Start the AVR timer to generate ticks for background processing.
  //// debug("arduino_start_timer"); ////
  arduino_start_timer(); ////

  //// debug("os_start"); ////
  os_start();  //  Never returns.
  
	return EXIT_SUCCESS;
}
