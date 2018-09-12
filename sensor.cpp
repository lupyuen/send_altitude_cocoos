//  Defines the common Sensor base class.  Every Sensor is assumed to have a name comprising
//  3 lowercase letters and digits, e.g. "tmp".  The Sensor is capable of producing
//  one, two or three float values as sensor data, in a single sample.  The Sensor instance
//  for specific sensor (e.g. temp_sensor) will provide the function to poll the actual sensor.
//  Every Sensor will also post a SensorMsg to the Network Task or Display Task for
//  aggregation/transmission or for display.
////#define DISABLE_DEBUG_LOG  //  Disable debug logging.
#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "sensor.h"
#include "display.h"

#ifdef SENSOR_DATA

#ifdef STM32  //  If STM32 Blue Pill...
//  We call Simulator Module to capture, replay and simulate SPI commands for SPI sensors.
//  We do this so that we can capture the SPI send/receive commands of Arduino sensor drivers and replay
//  them efficiently on STM32, with multitasking.
#include <simulator.h>

#else  //  If Arduino or other platform...
//  No need to simulate on Arduino.
#define simulator_setup() {}
#define simulator_configure(sim, id, name) {}
#define simulator_open(sim) {}
#define simulator_close(sim) {}
#define simulator_test(sim) {}
#define simulator_should_poll_sensor(sim) true  //  Always poll the sensor.
#endif  //  STM32

uint8_t nextSensorID = 1;  //  Next sensor ID to be allocated.  Running sequence number.

void setup_sensor_context(
  SensorContext *context,
  Sensor *sensor,
  uint16_t pollInterval,
  uint8_t taskID
  ) {
  //  Set up the sensor context and call the sensor to initialise itself.

  //  Set up the simulator system once.
  simulator_setup();

  //  Allocate a unique sensor ID and create the event.
  uint8_t sensorID =  nextSensorID++;
  Evt_t event = event_create();

  //  Initialise the sensor values.
  sensor->info.id = sensorID; 
  sensor->info.event = &event;
  sensor->info.poll_interval = pollInterval;

  //  Set the context.
  context->sensor = sensor;
  context->receive_task_id = taskID;

  //  Call the sensor to do initialisation.
  sensor->control.init_sensor_func();

  //  Set up the simulator and SPI port for the sensor.
  simulator_configure(&sensor->simulator, sensorID, sensor->info.name, sensor->port);
}

void sensor_task(void) {
  //  Background task to receive and process sensor data.
  //  This task will be reused by all sensors: temperature, humidity, altitude.
  //  Don't declare any static variables inside here because they will conflict
  //  with other sensors.
  SensorContext *context = NULL;  //  Declared outside the task to prevent cross-initialisation error in C++.
  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) {  //  Run the sensor processing code forever. So the task never ends.
    //  We should not make this variable static, because the task data should be unique for each task.
    context = (SensorContext *) task_get_data();

    //  This code is executed by multiple sensors. We use a global semaphore to prevent 
    //  concurrent access to the single shared I2C Bus on Arduino Uno.
    debug(context->sensor->info.name, F(" >> Wait for semaphore")); ////
    sem_wait(i2cSemaphore);  //  Wait until no other sensor is using the I2C Bus. Then lock the semaphore.
    debug(context->sensor->info.name, F(" >> Got semaphore")); ////

    //  We have to fetch the context pointer again after the wait.
    context = (SensorContext *) task_get_data();

    //  Prepare a sensor data message for copying the sensor data.
    //  TODO: Save and retrieve msg from context.
    SensorMsg msg;
    msg.super.signal = context->sensor->info.id;  //  e.g. TEMP_DATA, GYRO_DATA.
    strncpy(msg.name, context->sensor->info.name, MAX_SENSOR_NAME_SIZE);  //  Set the sensor name e.g. tmp
    msg.name[MAX_SENSOR_NAME_SIZE] = 0;  //  Terminate the name in case of overflow.

    //  Begin to capture, replay or simulate the sensor SPI commands.
    simulator_open(&context->sensor->simulator);

    //  If this is the first time we are polling the sensor, or if this is a simulated sensor...
    if (simulator_should_poll_sensor(&context->sensor->simulator)) {
      //  Poll for the sensor data and copy into the sensor message.  This will also capture or simulate the sensor SPI commands.
      msg.count = context->sensor->info.poll_sensor_func(msg.data, MAX_SENSOR_DATA_SIZE);
    } else {
      //  Else we are replaying a captured SPI command.
      msg.count = 0;  //  Don't return the message yet until the simulation next round.
      volatile Evt_t *replay_event;
      for (;;) {  //  Replay every captured SPI packet and wait for the replay to the completed.
        replay_event = simulator_replay(&context->sensor->simulator);  //  Replay the next packet if any.
        if (replay_event == NULL) { break; }  //  No more packets to replay.
        debug_print(context->sensor->info.name); debug_println(F(" >> Wait for replay")); ////

        event_wait(*replay_event);  //  Wait for replay to complete.
        context = (SensorContext *) task_get_data();  //  Must refetch the context pointer after event_wait();
        debug(context->sensor->info.name, F(" >> Replay done")); ////
      }
    }

    //  End the capture, replay or simulation of the sensor SPI commands.
    simulator_close(&context->sensor->simulator);

    //  We are done with the I2C Bus.  Release the semaphore so that another task can fetch the sensor data.
    debug(context->sensor->info.name, F(" >> Release semaphore")); ////
    sem_signal(i2cSemaphore);
    context = (SensorContext *) task_get_data();  //  Fetch the context pointer again after releasing the semaphore.
    //  TODO: Save and retrieve msg from context.

    //  Do we have new data?
    if (msg.count > 0) {
      //  If we have new data, send to Network Task or Display Task. Note: When posting a message, its contents are cloned into the message queue.
      //  debug(msg.name, F(" >> Send msg")); ////
      debug_print(msg.name); debug_print(F(" >> Send msg ")); 
      if (msg.count > 0) { debug_println(msg.data[0]); }
      else { debug_println("(empty)"); }
      debug_flush();
      //  Note: msg_post() will block if the receiver's queue is full.
      msg_post_async(context->receive_task_id, msg);
    }

    //  Wait a short while before polling the sensor again.
    debug(context->sensor->info.name, F(" >> Wait interval")); ////
    task_wait(context->sensor->info.poll_interval);
  }
  debug(F("task_close"), NULL); ////
  task_close();  //  End of the task. Should never come here.
}

uint8_t receive_sensor_data(float *sensorDataArray, uint8_t sensorDataSize, float *data, uint8_t size) {
  //  Copy the received sensor data array into the provided data buffer.
  //  Return the number of floats copied.
  //// debug(F("receive_sensor_data")); ////
  uint8_t i;
  //  Copy the floats safely: Don't exceed the array size provided by caller.
  //  Also don't exceed the number of available sensor data items.
  for (i = 0; i < size && i < sensorDataSize && i < MAX_SENSOR_DATA_SIZE; i++) {
    data[i] = sensorDataArray[i];
  }
  return i;  //  Return the number of floats copied.
}

//  SensorInfo constructor for C++ only.
SensorInfo::SensorInfo(
  const char name0[],
  uint8_t (*poll_sensor_func0)(float *data, uint8_t size)
) {
  name = name0;
  poll_sensor_func = poll_sensor_func0;
}

//  SensorInfo constructor for C++ only.
SensorControl::SensorControl(
  void (*init_sensor_func0)(void),
  void (*next_channel_func0)(void),
  void (*prev_channel_func0)(void)
) {
  init_sensor_func = init_sensor_func0;
  next_channel_func = next_channel_func0;
  prev_channel_func = prev_channel_func0;
}

//  Sensor constructor for C++ only.
Sensor::Sensor(
  const char name[],
  void (*init_sensor_func)(void),
  uint8_t (*poll_sensor_func)(float *data, uint8_t size),
  void (*next_channel_func)(void),
  void (*prev_channel_func)(void)
): 
  info(name, poll_sensor_func),
  control(init_sensor_func, next_channel_func, prev_channel_func) {
}

#endif  //  SENSOR_DATA
