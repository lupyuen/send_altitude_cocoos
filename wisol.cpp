//  Functions to send and receive messages to/from Sigfox network via 
//  Wisol modules WSSFM10R1AT, WSSFM10R2AT, WSSFM10R3AT and WSSFM10R4AT
//  for Sigfox zones RCZ1, RCZ2, RCZ3 and RCZ4 respectively.
#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"
#include "aggregate.h"
#include "downlink.h"
#include "wisol.h"

#include "radio.h"


static void processResponse(NetworkContext *context);
static void processPendingResponse(NetworkContext *context);
static void getStepPowerChannel(NetworkContext *context, NetworkCmd list[], int listSize);
bool getID(NetworkContext *context, const char *response);
bool getPAC(NetworkContext *context, const char *response);
bool checkChannel(NetworkContext *context, const char *response);
bool getDownlink(NetworkContext *context, const char *response0);
static void createSensorMsg(SensorMsg *msg, const char *name);
static void addCmd(NetworkCmd list[], int listSize, NetworkCmd cmd);
static int getCmdIndex(NetworkCmd list[], int listSize);
static void convertCmdToUART(
  NetworkCmd *cmd,
  NetworkContext *context,
  RadioMsg *radioMsg,
  Evt_t successEvent0, 
  Evt_t failureEvent0,
  SensorMsg *responseMsg,
  uint8_t responseTaskID);

NetworkCmd endOfList = { NULL, 0, NULL, NULL, NULL };  //  Command to indicate end of command list.

static Sem_t sendSemaphore;
static Evt_t successEvent;
static Evt_t failureEvent;

static SensorMsg msg;  //  Incoming sensor data message.
static SensorMsg responseMsg;  //  Pending response message from UART to Wisol.
static RadioMsg radioMsg;  //  Outgoing UART message containing Wisol command.
static SensorMsg sendMsg; // Sent periodically by the network task itself to trigger a radio transmission

//void network_task(void) {
//  //  Loop forever, receiving sensor data messages and sending to UART Task to transmit to the network.
//  //  Note: Declare task variables here before the task but don't populate them here
//  //  unless they are not supposed to change.
//  //  Because the coroutine will execute this code repeatedly and repopulate the values.
//  NetworkContext *context;
//  NetworkCmd *cmd;
//  bool shouldSend;
//
//  //  Task Starts Here  ///////////////////////////////////
//  task_open();  //  Start of the task. Must be matched with task_close().
//  context = (NetworkContext *) task_get_data();
//  successEvent = event_create();  //  Create event for UART Task to indicate success.
//  failureEvent = event_create();  //  Another event to indicate failure.
//  createSensorMsg(&msg, BEGIN_SENSOR_NAME);  //  We create a "begin" message and process only upon startup.
//  createSensorMsg(&responseMsg, RESPONSE_SENSOR_NAME);  //  UART Task sends this message for a pending response received.
//
//  for (;;) {  //  Run the sensor data receiving code forever. So the task never ends.
//    //  If not the first iteration, wait for an incoming message containing sensor data.
//    if (strncmp(msg.name, BEGIN_SENSOR_NAME, MAX_SENSOR_NAME_SIZE) != 0) {
//      msg_receive(os_get_running_tid(), &msg);
//    }
//    context = (NetworkContext *) task_get_data();  //  Must fetch again after msg_receive().
//    //  If this is a UART response message, process the pending response.
//    if (strncmp(msg.name, RESPONSE_SENSOR_NAME, MAX_SENSOR_NAME_SIZE) == 0) {
//      processPendingResponse(context);
//      continue;
//    }
//    //  Aggregate the sensor data.  Determine whether we should send to network now.
//    cmdList[0] = endOfList;  //  Empty the command list.
//    shouldSend = aggregate_sensor_data(context, &msg, cmdList, MAX_NETWORK_CMD_LIST_SIZE);  //  Fetch the command list into cmdList.
//    if (!shouldSend) continue;  //  Should not send now. Loop and wait for next message.
//
//    //  Use a semaphore to limit sending to only 1 message at a time, because our buffers are shared.
//    debug(F("net >> Wait for net")); ////
//    sem_wait(sendSemaphore);  //  Wait until no other message is being sent. Then lock the semaphore.
//    context = (NetworkContext *) task_get_data();  //  Must get context after sem_wait();
//    debug(F("net >> Got net")); ////
//
//    //  Init the context.
//    context->status = true;  //  Assume no error.
//    context->pendingResponse = false;  //  Assume no need to wait for response.
//    context->msg = &msg;  //  Remember the message until it's sent via UART.
//    context->downlinkData = NULL;  //  No downlink received yet.
//    context->cmdList = cmdList;  //  Run the command list.
//    context->cmdIndex = 0;  //  Start at first command in command list.
//    context->lastSend = millis() + MAX_TIMEOUT;  //  Prevent other requests from trying to send.
//
//    for (;;) {  //  Send each Wisol AT command in the list.
//      context = (NetworkContext *) task_get_data();  //  Must get context to be safe.
//      context->lastSend = millis();  //  Update the last send time.
//
//      if (context->cmdIndex >= MAX_NETWORK_CMD_LIST_SIZE) { break; }  //  Check bounds.
//      cmd = &(context->cmdList[context->cmdIndex]);  //  Fetch the current command.
//      if (cmd->sendData == NULL) { break; }  //  No more commands to send.
//
//      //  Convert Wisol command to UART command.
//      convertCmdToUART(cmd, context, &radioMsg, successEvent, failureEvent, &responseMsg, os_get_running_tid());
//      context->lastSend = millis() + radioMsg.timeout;  //  Estimate last send time for the next command.
//      context->pendingProcessFunc = cmd->processFunc;  //  Call this function to process response later.
//
//      //  Send the UART command thru the UART Task.
//      //  msg_post() is a synchronised send - it waits for the queue to be available before sending.
//      msg_post(context->radioTaskID, radioMsg);  //  Send the message to the UART task for transmission.
//      context = (NetworkContext *) task_get_data();  //  Must get context in case msg_post(blocks)
//
//      //  For sending payload: Break out of the loop and release the semaphore lock.
//      //  This allows other tasks to run.
//      if (radioMsg.responseMsg != NULL) {
//        context->pendingResponse = true;
//        break;
//      }
//      //  Wait for success or failure then process the response.
//      event_wait_multiple(0, successEvent, failureEvent);  //  0 means wait for any event.
//      context = (NetworkContext *) task_get_data();  //  Must get context after event_wait_multiple().
//      processResponse(context);  //  Process the response by calling the response function.
//      if (context->status == false) break;  //  Quit if the processing failed.
//
//      //  Command was successful. Move to next command.
//      context->cmdIndex++;  //  Next Wisol command.
//    }  //  Loop to next Wisol command.
//    //  All Wisol AT commands sent for the step.
//
//    //  Clean up the context and release the semaphore.
//    msg.name[0] = 0;  //  Erase the "begin" sensor name.
//    context->msg = NULL;  //  Erase the message.
//    context->cmdList = NULL;  //  Erase the command list.
//
//    //  Release the semaphore and allow another payload to be sent after SEND_INTERVAL.
//    debug(F("net >> Release net")); ////
//    sem_signal(sendSemaphore);
//    context = (NetworkContext *) task_get_data();  //  Must get context after sem_signal();
//  }  //  Loop to next incoming sensor data message.
//  task_close();  //  End of the task. Should not come here.
//}

//  Send a message to radio every 20,000 milliseconds = 20 seconds.
#define SEND_INTERVAL ((unsigned long) 20 * 1000)


void network_task(void) {
  //  Loop forever, receiving sensor data messages and sending to UART Task to transmit to the network.
  //  Note: Declare task variables here before the task but don't populate them here
  //  unless they are not supposed to change. 
  //  Because the coroutine will execute this code repeatedly and repopulate the values.
  NetworkContext *context;

  task_open();
  sendMsg.super.signal = TRANSMIT_SIG;
  msg_post_every(os_get_running_tid(), sendMsg, SEND_INTERVAL);

  context = (NetworkContext *) task_get_data();

  for (;;) {

    // Wait for message: it could be a sensor message or the periodic Send message
    msg_receive(os_get_running_tid(), &msg);

    context = (NetworkContext *) task_get_data();

    // will only be done once
    if (!context->initialized) {
      // the empty sendData will signal to the radio its time to initialize
      context->initialized = true;
      radioMsg.sendData = NULL;
      msg_post(context->radioTaskID, radioMsg);
      continue;
    }

    if (msg.super.signal == TRANSMIT_SIG) {
      // fetch latest sensor data
      aggregate_getSensorData(&radioMsg.sensorData[0]);

      radioMsg.sendData = &radioMsg.sensorData[0];
      // send it to the radio
      msg_post_async(context->radioTaskID, radioMsg);
    }
    else if (msg.super.signal == SENSOR_DATA_SIG) {
      // save the received sensor data
      aggregate_saveSensorData(&msg);
    }
  }

  task_close();  //  End of the task. Should not come here.
}



//static void processPendingResponse(NetworkContext *context) {
//  //  If there is a pending response, e.g. from send payload...
//  debug(F("net >> Pending response")); ////
//  if (!context->pendingResponse) {
//    debug(F("***** Error: No pending response"));
//    return;
//  }
//  context->pendingResponse = false;
//  processResponse(context);  //  Process the response by calling the response function.
//  context->lastSend = millis();  //  Update the last send time.
//  //  Process the downlink message, if any. This is located outside the semaphore lock for performance.
//  if (context->downlinkData) {
//    process_downlink_msg(context, context->status, context->downlinkData);
//  }
//}







/* Downlink Server Support: https://backend.sigfox.com/apidocs/callback
When a message needs to be acknowledged, the callback selected for the downlink data must 
send data in the response. It must contain the 8 bytes data that will be sent to the device 
asking for acknowledgment. The data is json formatted, and must be structured as the following :
  {"YOUR_DEVICE_ID" : { "downlinkData" : "deadbeefcafebabe"}}    
With YOUR_DEVICE_ID being replaced by the corresponding device id, in hexadecimal format, up to 8 digits. 
The downlink data must be 8 bytes in hexadecimal format.  For example:
  {"002C2EA1" : { "downlinkData" : "0102030405060708"}} */






static const char *cmdData;

//static void convertCmdToUART(
//  NetworkCmd *cmd,
//  NetworkContext *context,
//  RadioMsg *radioMsg,
//  Evt_t successEvent0,
//  Evt_t failureEvent0,
//  SensorMsg *responseMsg,
//  uint8_t responseTaskID) {
//  //  Convert the Wisol command into a UART message.
//  radioMsg->sendData = radioData;
//  radioData[0] = 0;  //  Clear the dest buffer.
//  radioMsg->timeout = COMMAND_TIMEOUT;
//  radioMsg->responseMsg = NULL;
//
//  if (cmd->sendData != NULL) {
//    //  Append sendData if it exists.  Need to use String class because sendData is in flash memory.
//    cmdData = cmd->sendData;
//#ifdef ARDUINO
//    const char *cmdDataStr = cmdData.c_str();
//#else
//    const char *cmdDataStr = cmdData;
//#endif  //  ARDUINO
//    strncpy(radioData, cmdDataStr, MAX_RADIO_SEND_MSG_SIZE - strlen(radioData));  //  Copy the command string.
//    radioData[MAX_RADIO_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
//  }
//  if (cmd->payload != NULL) {
//    //  Append payload if it exists.
//    strncat(radioData, cmd->payload, MAX_RADIO_SEND_MSG_SIZE - strlen(radioData));
//    radioData[MAX_RADIO_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
//    radioMsg->timeout = UPLINK_TIMEOUT;  //  Increase timeout for uplink.
//    //  If there is payload to send, send the response message when sending is completed.
//    radioMsg->responseMsg = responseMsg;
//    radioMsg->responseTaskID = responseTaskID;
//  }
//  if (cmd->sendData2 != NULL) {
//    //  Append sendData2 if it exists.  Need to use String class because sendData is in flash memory.
//    cmdData = cmd->sendData2;
//#ifdef ARDUINO
//    const char *cmdDataStr = cmdData.c_str();
//#else
//    const char *cmdDataStr = cmdData;
//#endif  //  ARDUINO
//    strncat(radioData, cmdDataStr, MAX_RADIO_SEND_MSG_SIZE - strlen(radioData));
//    radioData[MAX_RADIO_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
//    radioMsg->timeout = DOWNLINK_TIMEOUT;  //  Increase timeout for downlink.
//  }
//  //  Terminate the command with "\r".
//  strncat(radioData, CMD_END, MAX_RADIO_SEND_MSG_SIZE - strlen(radioData));
//  radioData[MAX_RADIO_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
//  //  debug(F("uartData="), uartData);  ////
//  //  Check total msg length.
//  if (strlen(radioData) >= MAX_RADIO_SEND_MSG_SIZE - 1) {
//    debug_print(F("***** Error: uartData overflow - ")); debug_print(strlen(radioData));
//    debug_print(" / "); debug_println(radioData); debug_flush();
//  }
//  radioMsg->markerChar = END_OF_RESPONSE;
//  radioMsg->expectedMarkerCount = cmd->expectedMarkerCount;
//  radioMsg->successEvent = successEvent0;
//  radioMsg->failureEvent = failureEvent0;
//}

void setup_wisol(
  NetworkContext *context,
  RadioContext *radioContext,
  int8_t radioTaskID,
  Country country0, 
  bool useEmulator0) {
  //  Init the Wisol context.

  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  sendSemaphore = sem_counting_create( maxCount, initValue );

  context->radioContext = radioContext;
  context->radioTaskID = radioTaskID;
  context->country = country0;
  context->useEmulator = useEmulator0;
  //context->stepBeginFunc = getStepBegin;
  //context->stepSendFunc = getStepSend;
  context->device[0] = 0;  //  Clear the device ID.
  context->pac[0] = 0;  //  Clear the PAC code.
  context->zone = context->country & RCZ_MASK;  //  Extract the zone from country node.
  context->lastSend = millis() + SEND_INTERVAL + SEND_INTERVAL;  //  Init the last send time to a high number so that sensor data will wait for Begin Step to complete.
  context->pendingResponse = false;
  context->initialized = false;
}

