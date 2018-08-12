//  Functions to send and receive messages to/from Sigfox network via Wisol module.
#include <Arduino.h>
#include "cocoos_cpp.h"
#include "display.h"
#include "uart.h"
#include "wisol.h"

#define END_OF_RESPONSE '\r'  //  Character '\r' marks the end of response.
#define CMD_END "\r"

static int getCmdIndex(WisolCmd list[]);
static void getCmdBegin(WisolContext *context, WisolCmd list[]);  //  Fetch list of startup commands for Wisol.
static void getCmdSend(
  WisolContext *context, 
  WisolCmd list[], 
  const char *payload,
  bool downlinkMode);
static void convertCmdToUART(
  WisolCmd *cmd,
  WisolContext *context, 
  UARTMsg *uartMsg, 
  Evt_t successEvent, 
  Evt_t failureEvent);

static Evt_t successEvent;
static Evt_t failureEvent;
static WisolCmd cmdList[maxWisolCmdListSize];  //  Static buffer for storing command list.
static WisolCmd endOfList = { NULL, 0, NULL, NULL, NULL };  //  Command to indicate end of command list.

void wisol_task(void) {
  //  Loop forever, receiving sensor data messages and sending to Wisol task to transmit.
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.
  WisolContext *context;
  WisolCmd *cmd;
  static WisolMsg msg;  //  TODO
  static WisolMsg beginMsg;  //  First message that will be sent to self upon startup.
  static UARTMsg uartMsg;  //  TODO

  task_open();  //  Start of the task. Must be matched with task_close().  
  context = (WisolContext *) task_get_data();
  successEvent = event_create();  //  Create event for UART Task to indicate success.
  failureEvent = event_create();  //  Another event to indicate failure.
  context->status = true;  //  Assume no error.

  //  Init the first message that will be sent to self upon startup.
  beginMsg.count = 0;  //  No data.
  strncpy(beginMsg.name, beginSensorName, maxSensorNameSize);  //  Sensor name "000" denotes "begin" message.
  beginMsg.name[maxSensorNameSize] = 0;  //  Terminate the name in case of overflow.

  for (;;) { //  Receiving sensor data Run the data sending code forever. So the task never ends.
    context = (WisolContext *) task_get_data();
    //  On task startup, send "begin" message to self so that we can process the Wisol "begin" commands.
    if (context->firstTime) {
      context->firstTime = false;
      msg_post(os_get_running_tid(), beginMsg); //  Send the message to our own task.
      continue;  //  Process the next incoming message, which should be the "begin" message.
    }
    //  Wait for an incoming message containing sensor data.
    msg_receive(os_get_running_tid(), &msg);
    context = (WisolContext *) task_get_data();  //  Must fetch again after msg_receive().
    context->msg = &msg;  //  Remember the message until it's sent via UART.
    context->cmdList = cmdList;
    context->cmdIndex = 0;

    //  Convert received sensor data to a list of Wisol commands.
    cmdList[0] = endOfList;  //  Empty the command list.
    if (strncmp(context->msg->name, beginSensorName, maxSensorNameSize) == 0) {
      //  If sensor name is "000", this is the "begin" message.
      getCmdBegin(context, cmdList);  //  Fetch list of startup commands for Wisol.
    } else {
      //  TODO: Check whether we should transmit.
      static const char *payload = "0102030405060708090a0b0c";  //  TODO
      bool downlinkMode = false;
      getCmdSend(context, cmdList, payload, downlinkMode);
    }

    for (;;) {  //  Send each command in the list.
      context = (WisolContext *) task_get_data();  //  Must get context to be safe.
      if (context->cmdIndex >= maxWisolCmdListSize) { break; }  //  Check bounds.
      cmd = &(context->cmdList[context->cmdIndex]);  //  Fetch the current command.        
      if (cmd->sendData == NULL) { break; }  //  No more commands to send.

      //  Convert Wisol command to UART command and send it.
      convertCmdToUART(cmd, context, &uartMsg, successEvent, failureEvent);
      // debug(F("uartMsg.sendData2="), uartMsg.sendData);  ////
      msg_post(context->uartTaskID, uartMsg);  //  Send the message to the UART task for transmission.

      //  Wait for success or failure.
      event_wait_multiple(0, successEvent, failureEvent);  //  0 means wait for any event.
      context = (WisolContext *) task_get_data();  //  Must get context after event_wait_multiple().
      
      //  In case of failure, stop.
      if (context->uartContext->status != true) {
        debug(F("wisol_task: UART failed"));
        context->status = false;  //  Propagate status to Wisol context.
        break;  //  Quit processing.
      }
      //  Process the response.
      cmd = &context->cmdList[context->cmdIndex];
      if (cmd->processFunc != NULL) {
        const char *response = context->uartContext->response;
        debug(F("wisol_task: response = "), response);
        context->status = (cmd->processFunc)(context, response);
        //  If response processing failed, stop.
        if (context->status != true) {
          debug(F("wisol_task: Result processing failed"));
          break;  //  Quit processing.
        }
      }
      context->cmdIndex++;  //  Next Wisol command.
    }  //  Loop to next Wisol command.
  }  //  Loop to next incoming sensor data message.
  task_close();  //  End of the task. Should not come here.
}

#define CMD_NONE "AT"  //  Empty placeholder command.
#define CMD_OUTPUT_POWER_MAX "ATS302=15"  //  For RCZ1: Set output power to maximum power level.
#define CMD_PRESEND "AT$GI?"  //  For RCZ2, 4: Send this command before sending messages.  Returns X,Y.
#define CMD_PRESEND2 "AT$RC"  //  For RCZ2, 4: Send this command if presend returns X=0 or Y<3.
#define CMD_SEND_MESSAGE "AT$SF="  //  Prefix to send a message to SIGFOX cloud.
#define CMD_SEND_MESSAGE_RESPONSE ",1"  //  Expect downlink response from SIGFOX.
#define CMD_GET_ID "AT$I=10"  //  Get SIGFOX device ID.
#define CMD_GET_PAC "AT$I=11"  //  Get SIGFOX device PAC, used for registering the device.
#define CMD_GET_TEMPERATURE "AT$T?"  //  Get the module temperature.
#define CMD_GET_VOLTAGE "AT$V?"  //  Get the module voltage.
#define CMD_RESET "AT$P=0"  //  Software reset.
#define CMD_SLEEP "AT$P=1"  //  TODO: Switch to sleep mode : consumption is < 1.5uA
#define CMD_WAKEUP "AT$P=0"  //  TODO: Switch back to normal mode : consumption is 0.5 mA

#define CMD_RCZ1 "AT$IF=868130000"  //  EU / RCZ1 Frequency
#define CMD_RCZ2 "AT$IF=902200000"  //  US / RCZ2 Frequency
#define CMD_RCZ3 "AT$IF=902080000"  //  JP / RCZ3 Frequency
#define CMD_RCZ4 "AT$IF=920800000"  //  RCZ4 Frequency
#define CMD_MODULATION_ON "AT$CB=-1,1"  //  Modulation wave on.
#define CMD_MODULATION_OFF "AT$CB=-1,0"  //  Modulation wave off.
#define CMD_EMULATOR_DISABLE "ATS410=0"  //  Device will only talk to Sigfox network.
#define CMD_EMULATOR_ENABLE "ATS410=1"  //  Device will only talk to SNEK emulator.

bool getID(WisolContext *context, const char *response) {
  //  Save the device ID to context.
  strncpy(context->device, response, maxSigfoxDeviceSize);
  context->device[maxSigfoxDeviceSize] = 0;  //  Terminate the device ID in case of overflow.
  debug(F("getID: "), context->device);
  return true;
}

bool getPAC(WisolContext *context, const char *response) {
  //  Save the PAC code to context.
  strncpy(context->pac, response, maxSigfoxPACSize);
  context->pac[maxSigfoxPACSize] = 0;  //  Terminate the PAC code in case of overflow.
  debug(F("getPAC: "), context->pac);
  return true;
}

bool getDownlink(WisolContext *context, const char *response) {
  //  Extract the downlink message and write into response.
  debug(F("getDownlink: "), response);
  /* TODO:
  //  Successful response: OK\nRX=01 23 45 67 89 AB CD EF
  //  Timeout response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
  //  Remove the prefix and spaces.
  response.replace("OK\nRX=", "");
  response.replace(" ", "");
  */
  return true;
}

bool checkPower(WisolContext *context, const char *response) {  
  //  Parse the power response "X,Y" to determine if we need to send the second power command.
  //  If not needed, change the next command to CMD_NONE.
  //  debug(F("checkPower: "), response);
  if (strlen(response) < 3) {  //  If too short, return error.
    debug(F("checkPower Error: Unknown response "), response);
    return false;  //  Failure
  }
  //  Change chars to numbers.
  int x = response[0] - '0';
  int y = response[2] - '0';
  if (x != 0 && y >= 3) {
    //  No need to send second power command. We change CMD_PRESEND2 to CMD_NONE.
    debug(F("checkPower: change CMD_PRESEND2"));
    int cmdIndex = context->cmdIndex;  //  Current index.
    cmdIndex++;  //  Next index, to be updated.
    if (cmdIndex >= maxWisolCmdListSize) {      
      debug(F("checkPower Error: No cmd space"));  //  List is full.
      return false;  //  Failure
    }    
    if (context->cmdList[cmdIndex].sendData == NULL) {
      debug(F("checkPower Error: Empty cmd"));  //  Not supposed to be empty.
      return false;  //  Failure
    }
    context->cmdList[cmdIndex].sendData = F(CMD_NONE);
  } else {
    //  Continue to send CMD_PRESEND2
    debug(F("checkPower: continue CMD_PRESEND2"));
  }
  return true;  //  Success
}

static void getCmdBegin(WisolContext *context, WisolCmd list[]) {
  //  Return the list of UART commands to start up the Wisol module.
  int i = getCmdIndex(list);  //  Get next available index.
  //  Set emulation mode.
  list[i++] = {
    context->useEmulator  //  If emulator mode,
      ? F(CMD_EMULATOR_ENABLE)  //  Device will only talk to SNEK emulator.
      : F(CMD_EMULATOR_DISABLE),  //  Else device will only talk to Sigfox network.
    1, NULL, NULL, NULL };
  //  Get Sigfox device ID and PAC.
  list[i++] = { F(CMD_GET_ID), 1, getID, NULL, NULL };
  list[i++] = { F(CMD_GET_PAC), 1, getPAC, NULL, NULL };
  list[i++] = endOfList;
}

static void getCmdPower(WisolContext *context, WisolCmd list[]) {
  //  Set the output power for the zone.
  //  Get the command based on the zone.
  //  log2(F(" - Wisol.setOutputPower: zone "), String(zone));
  int i = getCmdIndex(list);  //  Get next available index.
  switch(context->zone) {
    case 1:  //  RCZ1
    case 3:  //  RCZ3
      //  Send CMD_OUTPUT_POWER_MAX
      list[i++] = { F(CMD_OUTPUT_POWER_MAX), 1, NULL, NULL, NULL };
      break;
    case 2:  //  RCZ2
    case 4: {  //  RCZ4
      //  Send CMD_PRESEND
      list[i++] = { F(CMD_PRESEND), 1, checkPower, NULL, NULL };
      //  Send second power command: CMD_PRESEND2.  
      //  Note: checkPower() may change this command to CMD_NONE if not required.
      list[i++] = { F(CMD_PRESEND2), 1, NULL, NULL, NULL };
      break;
    }
  }
  list[i++] = endOfList;
}

static void getCmdSend(
  WisolContext *context, 
  WisolCmd list[], 
  const char *payload,
  bool downlinkMode) {
  //  Return the list of UART commands to send the payload.
  //  Payload contains a string of hex digits, up to 24 digits / 12 bytes.
  //  We prefix with AT$SF= and send to the transceiver.
  //  If downlinkMode is true, we append the
  //  CMD_SEND_MESSAGE_RESPONSE command to indicate that we expect a downlink repsonse.
  //  The downlink response message from Sigfox will be returned in the response parameter.
  //  Warning: This may take up to 1 min to run.

  //  Set the output power for the zone.
  getCmdPower(context, list);

  //  Compose the payload sending command.
  int i = getCmdIndex(list);  //  Get next available index.
  uint8_t markers = 1;  //  Wait for 1 line of response.
  bool (*processFunc)(WisolContext *context, const char *response) = NULL;  //  Function to process result.
  const __FlashStringHelper *sendData2 = NULL;  //  Text to be appended to payload.

  // If no downlink: Send CMD_SEND_MESSAGE + payload
  if (downlinkMode) {
    //  For downlink mode: send CMD_SEND_MESSAGE + payload + CMD_SEND_MESSAGE_RESPONSE
    markers++;  //  Wait for one more response line.   
    processFunc = getDownlink;  //  Process the downlink message.
    sendData2 = F(CMD_SEND_MESSAGE_RESPONSE);  //  Append suffix to payload.
  }
  list[i++] = { F(CMD_SEND_MESSAGE), markers, processFunc, payload, sendData2 };

  //  TODO: Throttle.

  list[i++] = endOfList;
}

static String cmdData;

static void convertCmdToUART(
  WisolCmd *cmd,
  WisolContext *context, 
  UARTMsg *uartMsg, 
  Evt_t successEvent0, 
  Evt_t failureEvent0) {
  //  Convert the Wisol command into a UART message.
  char *uartData = uartMsg->sendData;
  uartData[0] = 0;  //  Clear the dest buffer.

  if (cmd->sendData != NULL) {
    //  Append sendData if it exists.  Need to use String class because sendData is in flash memory.
    cmdData = cmd->sendData;
    const char *cmdDataStr = cmdData.c_str();
    // debug(F("strSendData="), strSendData);  ////
    // strncpy(uartData, "AT$I=10\r", maxUARTMsgLength - strlen(uartData));  //  For testing
    strncpy(uartData, cmdDataStr, maxUARTMsgLength - strlen(uartData));  //  Copy the command string.
    uartData[maxUARTMsgLength] = 0;  //  Terminate the UART data in case of overflow.
  }
  if (cmd->payload != NULL) {
    //  Append payload if it exists.
    strncat(uartData, cmd->payload, maxUARTMsgLength - strlen(uartData));
    uartData[maxUARTMsgLength] = 0;  //  Terminate the UART data in case of overflow.
  }
  if (cmd->sendData2 != NULL) {
    //  Append sendData2 if it exists.  Need to use String class because sendData is in flash memory.
    cmdData = cmd->sendData2;
    const char *cmdDataStr = cmdData.c_str();
    strncat(uartData, cmdDataStr, maxUARTMsgLength - strlen(uartData));
    uartData[maxUARTMsgLength] = 0;  //  Terminate the UART data in case of overflow.
  }
  //  Terminate the command with "\r".
  strncat(uartData, CMD_END, maxUARTMsgLength - strlen(uartData));
  uartData[maxUARTMsgLength] = 0;  //  Terminate the UART data in case of overflow.
  //  debug(F("uartData="), uartData);  ////
  //  Check total msg length
  if (strlen(uartData) >= maxUARTMsgLength - 1) {
    debug(F("Error: uartData overflow"));
  }

  uartMsg->timeout = COMMAND_TIMEOUT;
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
  context->uartContext = uartContext;
  context->uartTaskID = uartTaskID;
  context->country = country0;
  context->useEmulator = useEmulator0;
  context->device[0] = 0;  //  Clear the device ID.
  context->pac[0] = 0;  //  Clear the PAC code.
  context->firstTime = true;

  switch(context->country) {
    case COUNTRY_JP: context->zone = 3; break; //  Set Japan frequency (RCZ3).
    case COUNTRY_US: context->zone = 2; break; //  Set US frequency (RCZ2).
    case COUNTRY_FR:  //  France (RCZ1).
    case COUNTRY_OM:  //  Oman (RCZ1).
    case COUNTRY_SA:  //  South Africa (RCZ1).
      context->zone = 1; break;
    //  Rest of the world runs on RCZ4.
    default: context->zone = 4;
  }
}

static int getCmdIndex(WisolCmd list[]) {
  //  Given a list of commands, return the index of the next empty element.
  int i;
  for (i = 0;  //  Search all elements in list.
    list[i].sendData != NULL &&   //  Skip no-empty elements.
    i < maxWisolCmdListSize;  //  Don't exceed the list size.
    i++) {}
  if (i >= maxWisolCmdListSize) {
    //  List is full.
    debug(F("Error: Cmd list overflow"));
    i = maxWisolCmdListSize - 1;
  }
  return i;
}
