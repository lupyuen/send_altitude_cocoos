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

static Sem_t *allocate_port_semaphore(uint32_t port_id);
static bool is_valid_event_sensor(Sensor *sensor);

static uint8_t nextSensorID = 1;        //  Next sensor ID to be allocated.  Running sequence number.
static uint8_t portSemaphoreIndex = 0;  //  Next portSemaphore to be allocated.
static struct {      //  List of I/O ports and their semaphores to prevent concurrent port access.
  uint32_t port_id;  //  Port ID e.g. SPI1, I2C1
  Sem_t semaphore;   //  Semaphore to lock the port
} portSemaphores[MAX_PORT_COUNT];

void sensor_task(void) {
  //  Background task to receive and process sensor data.
  //  This task will be reused by all sensors: temperature, humidity, altitude.
  //  Don't declare any static variables inside here because they will conflict
  //  with other sensors.
  SensorContext *context = NULL;  //  Declared outside the task to prevent cross-initialisation error in C++.
  task_open();  //  Start of the task. Must be matched with task_close().
  context = (SensorContext *) task_get_data();
  if (context->read_semaphore == NULL) { debug("*** ERROR: Missing port semaphore"); return; }  //  Must have semaphore for locking the I/O port.

  for (;;) {  //  Run the sensor processing code forever. So the task never ends.    
    context = (SensorContext *) task_get_data();  //  Must refetch the context after task_wait().
    debug_print(context->sensor->info.name); debug_print(F(" >> Wait for semaphore #")); debug_println((int) *context->read_semaphore); debug_flush();

    //  This code is executed by multiple sensors. We use a semaphore to prevent 
    //  concurrent access to the shared I2C or SPI port on Arduino Uno or Blue Pill.
    sem_wait(*context->read_semaphore);  //  Wait until no other sensor is using the I/O port. Then lock the semaphore.
    context = (SensorContext *) task_get_data();  //  Must fetch the context pointer again after the wait.
    debug_print(context->sensor->info.name); debug_print(F(" >> Got semaphore #")); debug_println((int) *context->read_semaphore); debug_flush();
    
    //  Begin to capture, replay or simulate the sensor SPI commands.
    simulator_open(&context->sensor->simulator);
    context->msg.count = SENSOR_NOT_READY;  //  Assume that sensor has no data available.
    context->send_semaphore = NULL;         //  Assume no need to wait before sending sensor data.

    //  If this is the first time we are polling the sensor, or if this is a simulated sensor...
    if (simulator_should_poll_sensor(&context->sensor->simulator)) {
      //  Poll for the sensor data and copy into the sensor message.  For Simulator: This will also capture or simulate the sensor SPI commands.
      context->msg.count = context->sensor->info.poll_sensor_func(context->msg.data, MAX_SENSOR_DATA_SIZE);
      context->send_semaphore = &context->sensor->info.semaphore;  //  If sensor data not ready, wait for this sensor semaphore.
    }

    //  This loop is only used by Event Sensors to wait for data, or by the Simulator replaying multiple SPI packets.
    for (;;) {  //  Loop until sensor data is ready.
      if (context->msg.count != SENSOR_NOT_READY) { break; }  //  Stop if we already have data.
      if (context->send_semaphore) {         //  If there is a semaphore for us to wait before sending...
        sem_wait(*context->send_semaphore);  //  Wait for sensor processing or replay to complete.  TODO: Handle timeout.
        context = (SensorContext *) task_get_data();  //  Must refetch the context pointer after sem_wait().
      }
      if (simulator_should_poll_sensor(&context->sensor->simulator)) {  //  If this is a real sensor...
        //  Process any sensor data received. If processing is complete, get the sensor data.
        context->msg.count = context->sensor->info.resume_sensor_func(context->msg.data, MAX_SENSOR_DATA_SIZE);
      } else {  //  Else this is the Simulator.  Replay the next packet if any.
        context->send_semaphore = simulator_replay(&context->sensor->simulator);
        if (context->send_semaphore == NULL) { break; }  //  Stop if no more packets to replay.
      }
    }

    //  End the capture, replay or simulation of the sensor SPI commands.
    simulator_close(&context->sensor->simulator);

    //  We are done with the I/O port.  Release the semaphore so that another task can fetch the sensor data on the port.
    debug_print(context->sensor->info.name); debug_print(F(" >> Release semaphore #")); debug_println((int) *context->read_semaphore); debug_flush();
    sem_signal(*context->read_semaphore);
    context = (SensorContext *) task_get_data();  //  Fetch the context pointer again after releasing the semaphore.

    //  Do we have new data?
    if (context->msg.count > 0 && context->msg.count != SENSOR_NOT_READY) {
      //  If we have new data, send to Network Task or Display Task. Note: When posting a message, its contents are cloned into the message queue.
      debug_print(context->msg.name); debug_print(F(" >> Send msg ")); debug_println(context->msg.data[0]); debug_flush();
      //  Note: We use msg_post_async() instead because msg_post() will block if the receiver's queue is full.
      msg_post_async(context->receive_task_id, context->msg);
      context = (SensorContext *) task_get_data();  //  Must refetch the context pointer.
    }
    //  Wait a short while before polling the sensor again.
    debug(context->sensor->info.name, F(" >> Wait interval"));
    task_wait(context->sensor->info.poll_interval);
  }
  debug(F("task_close"), NULL);
  task_close();  //  End of the task. Should never come here.
}

static Sem_t *allocate_port_semaphore(uint32_t port_id) {
  //  Given a port ID (e.g. SPI1), allocate the semaphore to be used for locking the port.  Reuse if already allocated.
  //  This is to prevent concurrent access to the same I/O port.
  //  Port ID not found.  Allocate a new semaphore.
  if (port_id == 0) {
    debug(F("*** ERROR: Invalid port ID"));
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
    debug(F("*** ERROR: Port semaphore overflow. Increase MAX_PORT_COUNT"));
    return NULL;
  }
  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I/O port.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I/O port.
  portSemaphores[portSemaphoreIndex].port_id = port_id;
  portSemaphores[portSemaphoreIndex].semaphore = sem_counting_create(maxCount, initValue);
  Sem_t *result = &portSemaphores[portSemaphoreIndex].semaphore;
  portSemaphoreIndex++;
  return result;
}

uint8_t receive_sensor_data(float *sensorDataArray, uint8_t sensorDataSize, float *data, uint8_t size) {
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

void setup_sensor_context(
  SensorContext *context,
  Sensor *sensor,
  uint16_t pollInterval,
  uint8_t taskID
  ) {
  //  Set up the sensor context and call the sensor to initialise itself.

  //  Set up the simulator system once.
  simulator_setup();

  //  Allocate a unique sensor ID.
  uint8_t sensorID =  nextSensorID++;

  //  Initialise the sensor values.
  sensor->info.id = sensorID; 
  sensor->info.semaphore = sem_bin_create(0);
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

  //  For Simulator: By default we capture, replay and simulate commands sent the SPI port.
  bool capture_enabled = true; bool replay_enabled = true; bool simulate_enabled = true;

  //  For Event Sensors: Don't capture, replay and simulate the SPI commands.
  if (sensor->info.resume_sensor_func) {
    if (!is_valid_event_sensor(context->sensor)) { return; }  //  Stop if this is not an Event Sensor.
    capture_enabled = false; replay_enabled = false; simulate_enabled = false;
  }

  //  Set up the simulator and SPI port for the sensor.
  simulator_configure(&sensor->simulator, sensorID, sensor->info.name, sensor->port,
    capture_enabled, replay_enabled, simulate_enabled);
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
  uint8_t (*resume_sensor_func)(float *data, uint8_t size),
  bool (*is_sensor_ready_func)(void),
  void (*next_channel_func)(void),
  void (*prev_channel_func)(void)
): 
  info(name, poll_sensor_func, resume_sensor_func, is_sensor_ready_func),
  control(init_sensor_func, next_channel_func, prev_channel_func) {
}

#endif  //  SENSOR_DATA
