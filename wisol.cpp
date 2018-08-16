//  Functions to send and receive messages to/from Sigfox network via 
//  Wisol modules WSSFM10R1AT, WSSFM10R2AT, WSSFM10R3AT and WSSFM10R4AT
//  for Sigfox zones RCZ1, RCZ2, RCZ3 and RCZ4 respectively.
#include "platform.h"
#include <string.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "display.h"
#include "sensor.h"
#include "uart.h"
#include "aggregate.h"
#include "downlink.h"
#include "wisol.h"

#define END_OF_RESPONSE '\r'  //  Character '\r' marks the end of response.
#define CMD_END "\r"

static void processResponse(WisolContext *context);
static void processPendingResponse(WisolContext *context);
static void getStepPowerChannel(WisolContext *context, WisolCmd list[], int listSize);
bool getID(WisolContext *context, const char *response);
bool getPAC(WisolContext *context, const char *response);
bool checkChannel(WisolContext *context, const char *response);
bool getDownlink(WisolContext *context, const char *response0);
static void createSensorMsg(SensorMsg *msg, const char *name);
static void addCmd(WisolCmd list[], int listSize, WisolCmd cmd);
static int getCmdIndex(WisolCmd list[], int listSize);
static void convertCmdToUART(
  WisolCmd *cmd,
  WisolContext *context, 
  UARTMsg *uartMsg, 
  Evt_t successEvent0, 
  Evt_t failureEvent0,
  SensorMsg *responseMsg,
  uint8_t responseTaskID);

WisolCmd endOfList = { NULL, 0, NULL, NULL, NULL };  //  Command to indicate end of command list.

static Sem_t sendSemaphore;
static Evt_t successEvent;
static Evt_t failureEvent;
static WisolCmd cmdList[MAX_WISOL_CMD_LIST_SIZE];  //  Static buffer for storing command list. Includes terminating msg.
static SensorMsg msg;  //  Incoming sensor data message.
static SensorMsg responseMsg;  //  Pending response message from UART to Wisol.
static UARTMsg uartMsg;  //  Outgoing UART message containing Wisol command.

void wisol_task(void) {
  //  Loop forever, receiving sensor data messages and sending to Wisol task to transmit.

  //  Note: Declare task variables here before the task but don't populate them here
  //  unless they are not supposed to change. 
  //  Because the coroutine will execute this code repeatedly and repopulate the values.
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.
  WisolContext *context;
  WisolCmd *cmd;
  bool shouldSend;

  //  Task Starts Here  ///////////////////////////////////
  task_open();  //  Start of the task. Must be matched with task_close().  
  context = (WisolContext *) task_get_data();
  successEvent = event_create();  //  Create event for UART Task to indicate success.
  failureEvent = event_create();  //  Another event to indicate failure.
  createSensorMsg(&msg, BEGIN_SENSOR_NAME);  //  We create a "begin" message and process only upon startup.
  createSensorMsg(&responseMsg, RESPONSE_SENSOR_NAME);  //  UART Task sends this message for a pending response received.

  for (;;) { //  Run the sensor data receiving code forever. So the task never ends.
    //  If not the first iteration, wait for an incoming message containing sensor data.
    if (strncmp(msg.name, BEGIN_SENSOR_NAME, MAX_SENSOR_NAME_SIZE) != 0) {
      msg_receive(os_get_running_tid(), &msg);
    }
    context = (WisolContext *) task_get_data();  //  Must fetch again after msg_receive().
    //  If this is a UART response message, process the pending response.
    if (strncmp(msg.name, RESPONSE_SENSOR_NAME, MAX_SENSOR_NAME_SIZE) == 0) {
      processPendingResponse(context);
      continue;
    }
    //  Aggregate the sensor data.  Determine whether we should send to network now.
    cmdList[0] = endOfList;  //  Empty the command list.
    shouldSend = aggregate_sensor_data(context, &msg, cmdList, MAX_WISOL_CMD_LIST_SIZE);  //  Fetch the command list into cmdList.
    if (!shouldSend) continue;  //  Should not send now. Loop and wait for next message.

    //  Use a semaphore to limit sending to only 1 message at a time, because our buffers are shared.
    debug(F("net >> Wait for net")); ////
    sem_wait(sendSemaphore);  //  Wait until no other message is being sent. Then lock the semaphore.
    context = (WisolContext *) task_get_data();  //  Must get context after sem_wait();
    debug(F("net >> Got net")); ////

    //  Init the context.
    context->status = true;  //  Assume no error.
    context->pendingResponse = false;  //  Assume no need to wait for response.
    context->msg = &msg;  //  Remember the message until it's sent via UART.  
    context->downlinkData = NULL;  //  No downlink received yet.  
    context->cmdList = cmdList;  //  Run the command list.
    context->cmdIndex = 0;  //  Start at first command in command list.
    context->lastSend = millis() + MAX_TIMEOUT;  //  Prevent other requests from trying to send.

    for (;;) {  //  Send each Wisol AT command in the list.
      context = (WisolContext *) task_get_data();  //  Must get context to be safe.
      context->lastSend = millis();  //  Update the last send time.

      if (context->cmdIndex >= MAX_WISOL_CMD_LIST_SIZE) { break; }  //  Check bounds.
      cmd = &(context->cmdList[context->cmdIndex]);  //  Fetch the current command.        
      if (cmd->sendData == NULL) { break; }  //  No more commands to send.

      //  Convert Wisol command to UART command and send it.
      convertCmdToUART(cmd, context, &uartMsg, successEvent, failureEvent, &responseMsg, os_get_running_tid());
      context->lastSend = millis() + uartMsg.timeout;  //  Estimate last send time for the next command.
      context->pendingProcessFunc = cmd->processFunc;  //  Call this function to process response later.

      //  msg_post() is a synchronised send - it waits for the queue to be available before sending.
      msg_post(context->uartTaskID, uartMsg);  //  Send the message to the UART task for transmission.
      context = (WisolContext *) task_get_data();  //  Must get context in case msg_post(blocks)

      //  For sending payload: Break out of the loop and release the semaphore lock. 
      //  This allows other tasks to run.
      if (uartMsg.responseMsg != NULL) {
        context->pendingResponse = true;
        break;
       }
      //  Wait for success or failure then process the response.
      event_wait_multiple(0, successEvent, failureEvent);  //  0 means wait for any event.
      context = (WisolContext *) task_get_data();  //  Must get context after event_wait_multiple().      
      processResponse(context);  //  Process the response by calling the response function.
      if (context->status == false) break;  //  Quit if the processing failed.

      //  Command was successful. Move to next command.
      context->cmdIndex++;  //  Next Wisol command.
    }  //  Loop to next Wisol command.
    //  All Wisol AT commands sent for the step.

    //  Clean up the context and release the semaphore.
    msg.name[0] = 0;  //  Erase the "begin" sensor name.
    context->msg = NULL;  //  Erase the message.
    context->cmdList = NULL;  //  Erase the command list.

    //  Release the semaphore and allow another payload to be sent after SEND_INTERVAL.
    debug(F("net >> Release net")); ////
    sem_signal(sendSemaphore);
    context = (WisolContext *) task_get_data();  //  Must get context after sem_signal();
  }  //  Loop to next incoming sensor data message.
  task_close();  //  End of the task. Should not come here.
}

static void processPendingResponse(WisolContext *context) {
  //  If there is a pending response, e.g. from send payload...
  debug(F("net >> Pending response")); ////
  if (!context->pendingResponse) {
    debug(F("***** Error: No pending response"));
    return;
  }
  context->pendingResponse = false;
  processResponse(context);  //  Process the response by calling the response function.
  context->lastSend = millis();  //  Update the last send time.
  //  Process the downlink message, if any. This is located outside the semaphore lock for performance.
  if (context->downlinkData) {
    processDownlinkMsg(context, context->status, context->downlinkData);
  }
}

static void processResponse(WisolContext *context) {
  //  Process the response from the Wisol AT Command by calling the
  //  process response function.  Set the status to false if the processing failed.

  //  Get the response text.
  const char *response = (context && context->uartContext)
    ? context->uartContext->response
    : "";          
  //  Process the response text, regardless of success/failure.
  //  Call the process response function if has been set.
  if (context->pendingProcessFunc != NULL) {
    bool processStatus = (context->pendingProcessFunc)(context, response);
    //  If response processing failed, stop.
    if (processStatus != true) {
      context->status = false;  //  Propagate status to Wisol context.
      debug(F("***** Error: wisol_task Result processing failed, response: "), response);
      return;  //  Quit processing.
    }
  }
  //  In case of failure, stop.
  if (context->uartContext->status != true) {
    context->status = false;  //  Propagate status to Wisol context.
    debug(F("***** Error: wisol_task Failed, response: "), response);
    return;  //  Quit processing.
  }
}

#ifdef NOTUSED
void sendTestSensorMsg() {
  createSensorMsg(&sensorMsg, "tmp");  //  Test message for temperature sensor.
  sensorMsg.data[0] = 36.9;
  sensorMsg.count = 1;
  //  Send test sensor message after 10 seconds (10,000 milliseconds).
  msg_post_in(os_get_running_tid(), sensorMsg, 10 * 1000); //  Send the message to our own task.
}
#endif

///////////////////////////////////////////////////////////////////////////////
//  Define the Wisol AT Commands based on WISOLUserManual_EVBSFM10RxAT_Rev.9_180115.pdf

#define CMD_NONE "AT"  //  Empty placeholder command.
#define CMD_OUTPUT_POWER_MAX "ATS302=15"  //  For RCZ1: Set output power to maximum power level.
#define CMD_GET_CHANNEL "AT$GI?"  //  For RCZ2, 4: Get current and next TX macro channel usage.  Returns X,Y.
#define CMD_RESET_CHANNEL "AT$RC"  //  For RCZ2, 4: Reset default channel. Send this command if CMD_GET_CHANNEL returns X=0 or Y<3.
#define CMD_SEND_MESSAGE "AT$SF="  //  Prefix to send a message to SIGFOX cloud.
#define CMD_SEND_MESSAGE_RESPONSE ",1"  //  Append to payload if downlink response from Sigfox is needed.
#define CMD_GET_ID "AT$I=10"  //  Get Sigfox device ID.
#define CMD_GET_PAC "AT$I=11"  //  Get Sigfox device PAC, used for registering the device.
#define CMD_EMULATOR_DISABLE "ATS410=0"  //  Device will only talk to Sigfox network.
#define CMD_EMULATOR_ENABLE "ATS410=1"  //  Device will only talk to SNEK emulator.

#ifdef NOTUSED  //  For future use.
#define CMD_GET_TEMPERATURE "AT$T?"  //  Get the module temperature.
#define CMD_GET_VOLTAGE "AT$V?"  //  Get the module voltage.

#define CMD_RESET "AT$P=0"  //  Software reset.
#define CMD_SLEEP "AT$P=1"  //  Switch to sleep mode : consumption is < 1.5uA
#define CMD_WAKEUP "AT$P=0"  //  Switch back to normal mode : consumption is 0.5 mA

#define CMD_RCZ1 "AT$IF=868130000"  //  Set EU / RCZ1 Frequency.
#define CMD_RCZ2 "AT$IF=902200000"  //  Set US / RCZ2 Frequency.
#define CMD_RCZ3 "AT$IF=902080000"  //  Set JP / RCZ3 Frequency.
#define CMD_RCZ4 "AT$IF=920800000"  //  Set RCZ4 Frequency.
#define CMD_MODULATION_ON "AT$CB=-1,1"  //  Modulation wave on.
#define CMD_MODULATION_OFF "AT$CB=-1,0"  //  Modulation wave off.
#endif

///////////////////////////////////////////////////////////////////////////////
//  Wisol Command Steps: A Command Step contains a list of Wisol AT Commands to
//  be sent for executing the step.  We only implement 2 steps for the Wisol module:
//  Begin Step -> Send Step
//  (1) Begin Step: On startup, set the emulation mode and get the device ID and PAC.
//  (2) Send Step: Send the payload, after setting the TX power and channel. Optional: Request for downlink

//  Each Wisol AT Command added through addCmd() may include a Response Processing
//  Function e.g. getID(), getPAC().  The function is called with the response text
//  generated from the Wisol AT Command.

void getStepBegin(
  WisolContext *context, 
  WisolCmd list[], 
  int listSize) {
  //  Return the list of Wisol AT commands for the Begin Step, to start up the Wisol module.
  //  debug(F(" - wisol.getStepBegin")); ////
  addCmd(list, listSize, {
    //  Set emulation mode.
    context->useEmulator  //  If emulator mode,
      ? F(CMD_EMULATOR_ENABLE)  //  Device will only talk to SNEK emulator.
      : F(CMD_EMULATOR_DISABLE),  //  Else device will only talk to Sigfox network.
    1, NULL, NULL, NULL });
  //  Get Sigfox device ID and PAC.
  addCmd(list, listSize, { F(CMD_GET_ID), 1, getID, NULL, NULL });
  addCmd(list, listSize, { F(CMD_GET_PAC), 1, getPAC, NULL, NULL });
}

void getStepSend(
  WisolContext *context, 
  WisolCmd list[],
  int listSize, 
  const char *payload,
  bool enableDownlink) {
  //  Return the list of Wisol AT commands for the Send Step, to send the payload.
  //  Payload contains a string of hex digits, up to 24 digits / 12 bytes.
  //  We prefix with AT$SF= and send to the transceiver.  If enableDownlink is true, we append the
  //  CMD_SEND_MESSAGE_RESPONSE command to indicate that we expect a downlink repsonse.
  //  The downlink response message from Sigfox will be returned in the response parameter.
  //  Warning: This may take up to 1 min to run.
  //  debug(F(" - wisol.getStepSend")); ////
  //  Set the output power for the zone.
  getStepPowerChannel(context, list, listSize);

  //  Compose the payload sending command.
  uint8_t markers = 1;  //  Wait for 1 line of response.
  bool (*processFunc)(WisolContext *context, const char *response) = NULL;  //  Function to process result.
  const __FlashStringHelper *sendData2 = NULL;  //  Text to be appended to payload.

  // If no downlink: Send CMD_SEND_MESSAGE + payload
  if (enableDownlink) {
    //  For downlink mode: send CMD_SEND_MESSAGE + payload + CMD_SEND_MESSAGE_RESPONSE
    markers++;  //  Wait for one more response line.   
    processFunc = getDownlink;  //  Process the downlink message.
    sendData2 = F(CMD_SEND_MESSAGE_RESPONSE);  //  Append suffix to payload.
  }
  addCmd(list, listSize, { F(CMD_SEND_MESSAGE), markers, processFunc, payload, sendData2 });
}

static void getStepPowerChannel(WisolContext *context, WisolCmd list[], int listSize) {
  //  Return the Wisol AT commands to set the transceiver output power and channel for the zone.
  //  See WISOLUserManual_EVBSFM10RxAT_Rev.9_180115.pdf, http://kochingchang.blogspot.com/2018/06/minisigfox.html
  //  debug(F(" - wisol.getStepPowerChannel")); ////
  switch(context->zone) {
    case RCZ1:
    case RCZ3:
      //  Set the transceiver output power.
      addCmd(list, listSize, { F(CMD_OUTPUT_POWER_MAX), 1, NULL, NULL, NULL });
      break;
    case RCZ2:
    case RCZ4: {
      //  Get the current and next macro channel usage. Returns X,Y:
      //  X: boolean value, indicating previous TX macro channel was in the Sigfox default channel
      //  Y: number of micro channel available for next TX request in current macro channel.
      //  Call checkChannel() to check the response.
      addCmd(list, listSize, { F(CMD_GET_CHANNEL), 1, checkChannel, NULL, NULL });

      //  If X=0 or Y<3, send CMD_RESET_CHANNEL to reset the device on the default Sigfox macro channel.
      //  Note: Don't use with a duty cycle less than 20 seconds.
      //  Note: checkChannel() will change this command to CMD_NONE if not required.
      addCmd(list, listSize, { F(CMD_RESET_CHANNEL), 1, NULL, NULL, NULL });
      break;
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
//  Wisol Response Processing Functions: Called to process response when response 
//  is received from Wisol AT Command.

bool getID(WisolContext *context, const char *response) {
  //  Save the device ID to context.
  strncpy(context->device, response, MAX_DEVICE_ID_SIZE);
  context->device[MAX_DEVICE_ID_SIZE] = 0;  //  Terminate the device ID in case of overflow.
  debug(F(" - wisol.getID: "), context->device);
  return true;
}

bool getPAC(WisolContext *context, const char *response) {
  //  Save the PAC code to context.  Note that the PAC is only valid
  //  for the first registration in the Sigfox portal.  After
  //  registering the device, the PAC is changed in the Sigfox portal
  //  but not in the Wisol AT Command.  You must get the updated
  //  PAC from the Sigfox portal if you wish to transfer the device.
  strncpy(context->pac, response, MAX_DEVICE_CODE_SIZE);
  context->pac[MAX_DEVICE_CODE_SIZE] = 0;  //  Terminate the PAC code in case of overflow.
  debug(F(" - wisol.getPAC: "), context->pac);
  return true;
}

bool checkChannel(WisolContext *context, const char *response) {  
  //  Parse the CMD_GET_CHANNEL response "X,Y" to determine if we need to send the CMD_RESET_CHANNEL command.
  //  If not needed, change the next command to CMD_NONE.

  //  CMD_GET_CHANNEL gets the current and next macro channel usage. Returns X,Y:
  //  X: boolean value, indicating previous TX macro channel was in the Sigfox default channel
  //  Y: number of micro channel available for next TX request in current macro channel.

  //  If X=0 or Y<3, send CMD_RESET_CHANNEL to reset the device on the default Sigfox macro channel.
  //  Note: Don't use with a duty cycle less than 20 seconds.
  //  debug(F("checkChannel: "), response);
  if (strlen(response) < 3) {  //  If too short, return error.
    debug(F("***** wisol.checkChannel Error: Unknown response "), response);
    return false;  //  Failure
  }
  //  Change chars to numbers.
  int x = response[0] - '0';
  int y = response[2] - '0';
  if (x != 0 && y >= 3) {
    //  No need to reset channel. We change CMD_RESET_CHANNEL to CMD_NONE.
    //  debug(F(" - wisol.checkChannel: Continue channel"));
    int cmdIndex = context->cmdIndex;  //  Current index.
    cmdIndex++;  //  Next index, to be updated.
    if (cmdIndex >= MAX_WISOL_CMD_LIST_SIZE) {      
      debug(F("***** wisol.checkChannel Error: Cmd overflow"));  //  List is full.
      return false;  //  Failure
    }    
    if (context->cmdList[cmdIndex].sendData == NULL) {
      debug(F("***** wisol.checkChannel Error: Empty cmd"));  //  Not supposed to be empty.
      return false;  //  Failure
    }
    context->cmdList[cmdIndex].sendData = F(CMD_NONE);
  } else {
    //  Continue to send CMD_RESET_CHANNEL
    //  debug(F(" - wisol.checkChannel: Reset channel"));
  }
  return true;  //  Success
}

/* Downlink Server Support: https://backend.sigfox.com/apidocs/callback
When a message needs to be acknowledged, the callback selected for the downlink data must 
send data in the response. It must contain the 8 bytes data that will be sent to the device 
asking for acknowledgment. The data is json formatted, and must be structured as the following :
  {"YOUR_DEVICE_ID" : { "downlinkData" : "deadbeefcafebabe"}}    
With YOUR_DEVICE_ID being replaced by the corresponding device id, in hexadecimal format, up to 8 digits. 
The downlink data must be 8 bytes in hexadecimal format.  For example:
  {"002C2EA1" : { "downlinkData" : "0102030405060708"}} */

bool getDownlink(WisolContext *context, const char *response0) {
  //  Extract the downlink message and write into the context response.
  //  context response will be returned as an 8-byte hex string, e.g. "0123456789ABCDEF"
  //  or a timeout error after 1 min e.g. "ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT"

  //  Get a writeable response pointer in the uartContext.
  char *response = context->uartContext->response;
  // debug(F(" - wisol.getDownlink: "), response); ////

  //  Check the original response.
  //  If Successful response: OK\nRX=01 23 45 67 89 AB CD EF
  //  -> Change response to: 0123456789ABCDEF
  //  If Timeout response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT\n
  //  -> Remove newline: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT

  //  Remove the prefix and spaces:
  //    replace "OK\nRX=" by "", replace " " by ""
  #define downlinkPrefix "OK\nRX="
  char *foundPtr = strstr(response, downlinkPrefix);
  if (foundPtr != NULL) {
    //  Found the delimiter. Transform <<BEFORE>>OK\nRX=<<AFTER>>
    //  To <<BEFORE>><<AFTER>>
    //  foundPtr points to "OK\nRX=".
    foundPtr[0] = 0;  //  Terminate <<BEFORE>>
    const char *after = foundPtr + strlen(downlinkPrefix);
    //  Shift <<AFTER>> next to <<BEFORE>>.
    strncat(response, after, MAX_UART_SEND_MSG_SIZE - strlen(response));
    response[MAX_UART_SEND_MSG_SIZE] = 0;  //  Terminate the response in case of overflow.
  } else {
    //  Return error e.g. ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
    context->status = false;
  }
  //  Remove all spaces.
  int src = 0, dst = 0;
  for (;;) {
    if (src >= MAX_UART_SEND_MSG_SIZE) break;
    //  Don't copy spaces and newlines in the source.
    if (response[src] == ' ' || response[src] == '\n') { 
      src++; 
      continue;
    }
    //  Copy only if the indexes are different.
    if (dst != src) { response[dst] = response[src]; }
    //  If we have copied the terminating null, quit.
    if (response[dst] == 0) break;
    dst++; src++;  //  Shift to next char.
  }
  response[MAX_UART_SEND_MSG_SIZE] = 0;  //  Terminate the response in case of overflow.
  context->downlinkData = response;
  return true;
}

static char uartData[MAX_UART_SEND_MSG_SIZE + 1];
#ifdef ARDUINO
static String cmdData;
#endif  //  ARDUINO

static void convertCmdToUART(
  WisolCmd *cmd,
  WisolContext *context, 
  UARTMsg *uartMsg, 
  Evt_t successEvent0, 
  Evt_t failureEvent0,
  SensorMsg *responseMsg,
  uint8_t responseTaskID) {
  //  Convert the Wisol command into a UART message.
  uartMsg->sendData = uartData;
  uartData[0] = 0;  //  Clear the dest buffer.
  uartMsg->timeout = COMMAND_TIMEOUT;
  uartMsg->responseMsg = NULL;

  if (cmd->sendData != NULL) {
    //  Append sendData if it exists.  Need to use String class because sendData is in flash memory.
#if defined(ARDUINO)    
    cmdData = cmd->sendData;
    const char *cmdDataStr = cmdData.c_str();
#elif defined(STM32)
    const char *cmdDataStr = cmd->sendData;
#endif
    strncpy(uartData, cmdDataStr, MAX_UART_SEND_MSG_SIZE - strlen(uartData));  //  Copy the command string.
    uartData[MAX_UART_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
  }
  if (cmd->payload != NULL) {
    //  Append payload if it exists.
    strncat(uartData, cmd->payload, MAX_UART_SEND_MSG_SIZE - strlen(uartData));
    uartData[MAX_UART_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
    uartMsg->timeout = UPLINK_TIMEOUT;  //  Increase timeout for uplink.
    //  If there is payload to send, send the response message when sending is completed.
    uartMsg->responseMsg = responseMsg;
    uartMsg->responseTaskID = responseTaskID;
  }
  if (cmd->sendData2 != NULL) {
    //  Append sendData2 if it exists.  Need to use String class because sendData is in flash memory.
#if defined(ARDUINO)    
    cmdData = cmd->sendData2;
    const char *cmdDataStr = cmdData.c_str();
#elif defined(STM32)
    const char *cmdDataStr = cmd->sendData;
#endif
    strncat(uartData, cmdDataStr, MAX_UART_SEND_MSG_SIZE - strlen(uartData));
    uartData[MAX_UART_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
    uartMsg->timeout = DOWNLINK_TIMEOUT;  //  Increase timeout for downlink.
  }
  //  Terminate the command with "\r".
  strncat(uartData, CMD_END, MAX_UART_SEND_MSG_SIZE - strlen(uartData));
  uartData[MAX_UART_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
  //  debug(F("uartData="), uartData);  ////
  //  Check total msg length.
  if (strlen(uartData) >= MAX_UART_SEND_MSG_SIZE - 1) {
    debug_print(F("***** Error: uartData overflow - ")); debug_print(strlen(uartData));
    debug_print(" / "); debug_println(uartData); debug_flush();
  }
  uartMsg->markerChar = END_OF_RESPONSE;
  uartMsg->expectedMarkerCount = cmd->expectedMarkerCount;
  uartMsg->successEvent = successEvent0;
  uartMsg->failureEvent = failureEvent0;
}

void setup_wisol(
  WisolContext *context, 
  UARTContext *uartContext, 
  int8_t uartTaskID, 
  Country country0, 
  bool useEmulator0) {
  //  Init the Wisol context.

  const int maxCount = 10;  //  Allow up to 10 tasks to queue for access to the I2C Bus.
  const int initValue = 1;  //  Allow only 1 concurrent access to the I2C Bus.
  sendSemaphore = sem_counting_create( maxCount, initValue );

  context->uartContext = uartContext;
  context->uartTaskID = uartTaskID;
  context->country = country0;
  context->useEmulator = useEmulator0;
  context->stepBeginFunc = getStepBegin;
  context->stepSendFunc = getStepSend;
  context->device[0] = 0;  //  Clear the device ID.
  context->pac[0] = 0;  //  Clear the PAC code.
  context->zone = context->country & RCZ_MASK;  //  Extract the zone from country node.
  context->lastSend = millis() + SEND_INTERVAL + SEND_INTERVAL;  //  Init the last send time to a high number so that sensor data will wait for Begin Step to complete.
  context->pendingResponse = false;
}

static void addCmd(WisolCmd list[], int listSize, WisolCmd cmd) {
  //  Append the UART message to the command list.
  //  Stop if we have overflowed the list.
  int i = getCmdIndex(list, listSize);
  list[i++] = cmd;
  list[i++] = endOfList;
}

static int getCmdIndex(WisolCmd list[], int listSize) {
  //  Given a list of commands, return the index of the next empty element.
  //  Check index against cmd size.  It must fit 2 more elements:
  //  The new cmd and the endOfList cmd.
  int i;
  for (i = 0;  //  Search all elements in list.
    list[i].sendData != NULL &&   //  Skip no-empty elements.
    i < listSize - 1;  //  Don't exceed the list size.
    i++) {}
  if (i >= listSize - 1) {
    //  List is full.
    debug_print(F("***** Error: Cmd list overflow - ")); debug_println(i); debug_flush();
    i = listSize - 2;
    if (i < 0) i = 0;
  }
  return i;
}

static void createSensorMsg(SensorMsg *msg, const char *name) {
  //  Populate the msg fields as an empty message.
  msg->count = 0;  //  No data.
  strncpy(msg->name, name, MAX_SENSOR_NAME_SIZE);
  msg->name[MAX_SENSOR_NAME_SIZE] = 0;  //  Terminate the name in case of overflow.
}
