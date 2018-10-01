//  Defines the common Sensor base class.
#include "platform.h"
#ifdef DISABLE_SENSOR_LOG
#define DISABLE_DEBUG_LOG  //  Disable debug logging for the Sensor Task.
#endif  //  DISABLE_SENSOR_LOG
#include <string.h>
#include <cocoos.h>
#include "sensor.h"
#include "display.h"

#ifdef SENSOR_DATA  //  If we are using real or simulated sensors instead of hardcoded sensor data...

#if defined(STM32) && defined(USE_SIMULATOR)        //  If we are running on STM32 Blue Pill and Simulator is enabled...
//  We call Simulator Module to capture, replay and simulate SPI commands for SPI sensors.
//  We do this so that we can capture the SPI send/receive commands of Arduino sensor drivers and replay
//  them efficiently on STM32, with multitasking.
#include <simulator.h>

#else  //  If Arduino or other platform or Simulator disabled...
//  No need to simulate on Arduino.
#define simulator_setup() {}
#define simulator_configure(sim, id, name, port, capture_enabled, replay_enabled, simulate_enabled, merge_enabled) {}
#define simulator_open(sim) {}
#define simulator_replay(sim) NULL
#define simulator_close(sim) {}
#define simulator_test(sim) {}
#define simulator_should_poll_sensor(sim) true  //  Always poll the sensor.
#endif  //  STM32 && USE_SIMULATOR

static Sem_t *allocate_port_semaphore(uint32_t port_id);
static bool is_valid_event_sensor(Sensor *sensor);

//  A Port Semaphore is a cocoOS Counting Semaphore that we use to prevent concurrent
//  access to an I/O port like I2C1, SPI1 or SPI2.  We store a global list of Port Semaphores
//  so that two sensors using the same port will be allocated the same Port Semaphore.
//  The Sensor Task will automatically wait for the Port Semaphore to be available
//  before polling the sensor.  The Sensor Task will also release the Port Semaphore
//  after polling/resuming the sensor.
static uint8_t nextSensorID = 1;        //  Next sensor ID to be allocated.  Running sequence number.
static uint8_t portSemaphoreIndex = 0;  //  Next portSemaphore to be allocated.
static struct {      //  List of I/O ports and their semaphores to prevent concurrent port access.
  uint32_t port_id;  //  Port ID e.g. SPI1, I2C1
  Sem_t semaphore;   //  Semaphore to lock the port
} portSemaphores[MAX_PORT_COUNT];

//  We define ctx() as a shortcut for fetching the SensorContext for the Sensor Task.
//  We use a macro instead of declaring a variable because the context needs to be refetched
//  after calling cocoOS functions that may switch the task context, e.g. sem_wait().
#define ctx() ((SensorContext *) task_get_data())

void sensor_task(void) {
  //  Background task to receive and process sensor data.  This task will be reused by 
  //  all sensors: temperature, humidity, altitude.  Don't declare any static variables inside here 
  //  because they will conflict with other sensors.
  task_open();  //  Start of the task. Must be matched with task_close().
  if (ctx()->read_semaphore == NULL) { debug("*** ERROR: Missing port semaphore", ""); return; }  //  Must have semaphore for locking the I/O port.

  for (;;) {  //  Run the sensor processing code forever. So the task never ends.    
    debug_print(ctx()->sensor->info.name); debug_print(F(" >> Wait for semaphore #")); debug_println((int) *ctx()->read_semaphore); // debug_flush();

    //  This code is executed by multiple sensors. We use a semaphore to prevent 
    //  concurrent access to the shared I2C or SPI port on Arduino Uno or Blue Pill.
    sem_wait(*ctx()->read_semaphore);  //  Wait until no other sensor is using the I/O port. Then lock the semaphore.
    debug_print(ctx()->sensor->info.name); debug_print(F(" >> Got semaphore #")); debug_println((int) *ctx()->read_semaphore); // debug_flush();
    
    //  Begin to capture, replay or simulate the sensor SPI commands.
    simulator_open(&ctx()->sensor->simulator);
    ctx()->msg.count = SENSOR_NOT_READY;  //  Assume that sensor has no data available.
    ctx()->send_semaphore = NULL;         //  Assume no need to wait before sending sensor data.

    //  If this is the first time we are polling the sensor, or if this is a simulated sensor...
    if (simulator_should_poll_sensor(&ctx()->sensor->simulator)) {
      //  Poll for the sensor data and copy into the sensor message.  For Simulator: This will also capture or simulate the sensor SPI commands.
      ctx()->msg.count = ctx()->sensor->info.poll_sensor_func(
        ctx()->msg.data, MAX_SENSOR_DATA_SIZE);  //  We will copy at most MAX_SENSOR_DATA_SIZE floats into the msg.data array.
      ctx()->send_semaphore = &ctx()->sensor->info.semaphore;  //  If sensor data not ready, wait for this sensor semaphore.
    }

    //  This loop is only used by Event Sensors to wait for data, or by the Simulator replaying multiple SPI packets.
    for (;;) {  //  Loop until sensor data is ready.
      if (ctx()->msg.count != SENSOR_NOT_READY) { break; }  //  Stop if we already have data.
      if (ctx()->send_semaphore) {         //  If there is a semaphore for us to wait before sending...
        sem_wait(*ctx()->send_semaphore);  //  Wait for sensor I/O (e.g. SPI port) or simulator replay to complete.  TODO: Handle timeout.
      }
      if (simulator_should_poll_sensor(&ctx()->sensor->simulator)) {  //  If this is a real sensor...
        //  Resume processing any sensor data received from the I/O port. If processing is complete, get the sensor data.
        ctx()->msg.count = ctx()->sensor->info.resume_sensor_func(
          ctx()->msg.data, MAX_SENSOR_DATA_SIZE);  //  We will copy at most MAX_SENSOR_DATA_SIZE floats into the msg.data array.
      } else {  //  Else this is the Simulator.  Replay the next packet if any.
        ctx()->send_semaphore = simulator_replay(&ctx()->sensor->simulator);
        if (ctx()->send_semaphore == NULL) { break; }  //  Stop if no more packets to replay.
      }
    }

    //  End the capture, replay or simulation of the sensor SPI commands.
    simulator_close(&ctx()->sensor->simulator);

    //  We are done with the I/O port.  Release the semaphore so that another task can fetch the sensor data on the port.
    debug_print(ctx()->sensor->info.name); debug_print(F(" >> Release semaphore #")); debug_println((int) *ctx()->read_semaphore); // debug_flush();
    sem_signal(*ctx()->read_semaphore);

    //  Do we have new data?
    if (ctx()->msg.count > 0 && ctx()->msg.count != SENSOR_NOT_READY) {
      //  If we have new data, send to Network Task or Display Task. Note: When posting a message, its contents are cloned into the message queue.
      debug_print(ctx()->msg.name); debug_print(F(" >> Send msg ")); debug_println(ctx()->msg.data[0]); // debug_flush();      
      msg_post_async(ctx()->receive_task_id, ctx()->msg);  //  Note: We use msg_post_async() instead because msg_post() will block if the receiver's queue is full.
    }
    //  Wait a short while before polling the sensor again.
    debug_print(ctx()->sensor->info.name); debug_println(F(" >> Wait interval"));
    task_wait(ctx()->sensor->info.poll_interval);
  }
  debug(F("task_close"), NULL);
  task_close();  //  End of the task. Should never come here.
}

static Sem_t *allocate_port_semaphore(uint32_t port_id) {
  //  Given a port ID (e.g. I2C1, SPI1, SPI2), allocate the Counting Semaphore to be used for locking the port.  
  //  Reuse if already allocated.  This semaphore prevents concurrent access to the same I/O port by 2 or more sensors.
  //  Port ID not found.  Allocate a new semaphore.
  if (port_id == 0) {
    debug(F("*** ERROR: Invalid port ID"), "");
    return NULL;
  }
  for (int i = 0; i < portSemaphoreIndex; i++) {
    //  Search for the port ID.
    if (port_id == portSemaphores[i].port_id) {
      return &portSemaphores[i].semaphore;
    }
  }
  //  Port ID not found.  Allocate a new semaphore.
  if (portSemaphoreIndex >= MAX_PORT_COUNT) {
    debug(F("*** ERROR: Port semaphore overflow. Increase MAX_PORT_COUNT"), "");
    return NULL;
  }
  //  Each I/O port (e.g. SPI1) is controlled by a Counting Semaphore.  Tasks will queue up to get access to the port.
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I/O port.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I/O port.
  portSemaphores[portSemaphoreIndex].port_id = port_id;
  portSemaphores[portSemaphoreIndex].semaphore = sem_counting_create(maxCount, initValue);
  Sem_t *result = &portSemaphores[portSemaphoreIndex].semaphore;
  portSemaphoreIndex++;
  return result;
}

void setup_sensor_context(
  SensorContext *context,  //  Context to be set up.
  Sensor *sensor,          //  Sensor to be set up.
  uint16_t pollInterval,   //  Polling interval in milliseconds.
  uint8_t taskID) {        //  Task ID for the Network or Display Task.  This task will receive sensor data messages.  
  //  Set up the sensor context. Allocate a new sensor ID and sensor semaphore.

  //  Set up the simulator system once.
  simulator_setup();

  //  Allocate a unique sensor ID.
  uint8_t sensorID = nextSensorID++;

  //  Initialise the sensor values.
  sensor->info.id = sensorID;
  sensor->info.semaphore = sem_bin_create(0);  //  Binary Semaphore: Will wait until signalled.
  sensor->info.poll_interval = pollInterval;
  sensor->port = NULL;
  sensor->port_id = 0;

  //  Set the context.
  context->sensor = sensor;
  context->receive_task_id = taskID;

  //  Call the sensor to do initialisation.  This should set the port and port ID.
  sensor->control.init_sensor_func();

  //  Allocate the semaphore for locking the I/O port (e.g. SPI1) so that multiple tasks won't access the port concurrently.
  context->read_semaphore = allocate_port_semaphore(sensor->port_id);
  if (context->read_semaphore == NULL) { return; }
  
  //  Prepare a sensor data message for sending the sensor data.
  context->msg.super.signal = context->sensor->info.id;
  strncpy(context->msg.name, context->sensor->info.name, MAX_SENSOR_NAME_SIZE);  //  Set the sensor name e.g. tmp
  context->msg.name[MAX_SENSOR_NAME_SIZE] = 0;  //  Terminate the name in case of overflow.

#ifdef USE_SIMULATOR  //  If we are using the Simulator...
  //  By default we capture, replay and simulate commands sent the SPI port.
  bool capture_enabled = true; bool replay_enabled = true; bool simulate_enabled = true; bool merge_enabled = true;

  //  For Event Sensors: Don't capture, replay and simulate the SPI commands.
  if (sensor->info.resume_sensor_func) {
    if (!is_valid_event_sensor(context->sensor)) { return; }  //  Stop if this is not an Event Sensor.
    capture_enabled = false; replay_enabled = false; simulate_enabled = false; merge_enabled = false;
  }
#else  //  Else we disable the simulator.
  bool capture_enabled = false; bool replay_enabled = false; bool simulate_enabled = false; bool merge_enabled = false;
#endif  //  USE_SIMULATOR

  //  Set up the simulator and SPI port for the sensor.
  simulator_configure(&sensor->simulator, sensorID, sensor->info.name, sensor->port,
    capture_enabled, replay_enabled, simulate_enabled, merge_enabled);
}

uint8_t receive_sensor_data(
  float *sensorDataArray,  //  Array of floats containing the received sensor data.
  uint8_t sensorDataSize,  //  Number of floats in the received sensor data.
  float *data,             //  Array of floats that sensor data should be copied to.
  uint8_t size) {          //  Number of floats in the array that sensor data should be copied to.
  //  Copy the received sensor data array into the provided data buffer.
  //  Return the number of floats copied.  //  debug(F("receive_sensor_data"));
  uint8_t i;
  //  Copy the floats safely: Don't exceed the array size provided by caller.
  //  Also don't exceed the number of available sensor data items.
  for (i = 0; i < size && i < sensorDataSize && i < MAX_SENSOR_DATA_SIZE; i++) {
    data[i] = sensorDataArray[i];
  }
  return i;  //  Return the number of floats copied.
}

static bool is_valid_event_sensor(Sensor *sensor) {
  //  Return true if this is a valid Event Sensor.
  if (sensor->info.resume_sensor_func == NULL) { debug(F("***** ERROR: Missing resume func for "), sensor->info.name); return false; }
  if (sensor->info.is_sensor_ready_func == NULL) { debug(F("***** ERROR: Missing sensor ready func for "), sensor->info.name); return false; }
  return true;
}

//  SensorInfo constructor for C++ only.
SensorInfo::SensorInfo(
  const char name0[],
  uint8_t (*poll_sensor_func0)(float *data, uint8_t size),
  uint8_t (*resume_sensor_func0)(float *data, uint8_t size),
  bool (*is_sensor_ready_func0)(void)
) {
  name = name0;
  poll_sensor_func = poll_sensor_func0;
  resume_sensor_func = resume_sensor_func0;
  is_sensor_ready_func = is_sensor_ready_func0;
}

//  SensorControl constructor for C++ only.
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
  uint8_t (*resume_sensor_func)(float *data, uint8_t size),
  bool (*is_sensor_ready_func)(void),
  void (*next_channel_func)(void),
  void (*prev_channel_func)(void)
): 
  info(name, poll_sensor_func, resume_sensor_func, is_sensor_ready_func),
  control(init_sensor_func, next_channel_func, prev_channel_func) {
}

#endif  //  SENSOR_DATA
