//  Functions to send and receive messages to/from Sigfox network via Wisol module.
#include <Arduino.h>
#include "cocoos_cpp.h"
#include "display.h"
#include "uart.h"
#include "wisol.h"

#define END_OF_RESPONSE '\r'  //  Character '\r' marks the end of response.
#define CMD_END "\r"

static void getBeginCmd(WisolContext *context, WisolCmd list[]);  //  Fetch list of startup commands for Wisol.
static void convertCmdToUART(
  WisolCmd *cmd,
  WisolContext *context, 
  UARTMsg *uartMsg, 
  Evt_t successEvent, 
  Evt_t failureEvent);

static Evt_t successEvent;
static Evt_t failureEvent;
static WisolCmd cmdList[maxWisolCmdListSize];  //  Static buffer for storing command list.

void wisol_task(void) {
  //  Loop forever, receiving sensor data messages and sending to Wisol task to transmit.
  Serial.begin(9600);  //  TODO

  WisolContext *context;
  WisolCmd *cmd;
  static WisolMsg msg;  //  TODO
  static UARTMsg uartMsg;  //  TODO
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.

  task_open();  //  Start of the task. Must be matched with task_close().
  context = (WisolContext *) task_get_data();
  successEvent = event_create();  //  Create event for UART Task to indicate success.
  failureEvent = event_create();  //  Another event to indicate failure.
  context->status = true;  //  Assume no error.

  for (;;) { //  Receiving sensor data Run the data sending code forever. So the task never ends.
    context = (WisolContext *) task_get_data();
    //  On task startup, send the UART commands to initialise the Wisol module.
    if (context->firstTime) {
      context->firstTime = false;
      getBeginCmd(context, cmdList);  //  Fetch list of startup commands for Wisol.
      for (;;) {  //  Send each command in the list.
        context = (WisolContext *) task_get_data();  //  Must get context to be safe.
        if (context->cmdIndex >= maxWisolCmdListSize) { break; }  //  Check bounds.
        cmd = &(context->cmdList[context->cmdIndex]);  //  Fetch the current command.        
        if (cmd->sendData == NULL) { break; }  //  No more commands to send.

        //  Convert Wisol command to UART command and send it.
        convertCmdToUART(cmd, context, &uartMsg, successEvent, failureEvent);
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
      }
      /*
      strncpy(uartMsg.sendData, "AT$I=10\r", maxUARTMsgLength);  //  TODO
      uartMsg.timeout = 1000;  //  TODO: COMMAND_TIMEOUT
      uartMsg.markerChar = '\r';  //  TODO: END_OF_RESPONSE
      uartMsg.expectedMarkerCount = 1;  //  TODO
      uartMsg.successEvent = event_create();  //  TODO
      uartMsg.failureEvent = event_create();  //  TODO
      msg_post(context->uartTaskID, uartMsg);  //  Send the message to the UART task for transmission.
      */
    }

    //  Wait for an incoming message containing sensor data to be transmitted.
    //// debug(F("msg_receive")); ////
    msg_receive(os_get_running_tid(), &msg);
    context = (WisolContext *) task_get_data();  //  Must fetch again after msg_receive().
    context->msg = &msg;  //  Remember the message until it's sent via UART.

    //  TODO: Check whether we should transmit.
  }
  task_close();  //  End of the task. Should not come here.
}

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

static WisolCmd endOfList = { NULL, 0, NULL };  //  Indicate end of command list.

bool getID(WisolContext *context, const char *response) {
  debug(F("getID: "), response);
  //  TODO
  return true;
}

bool getPAC(WisolContext *context, const char *response) {
  debug(F("getPAC: "), response);
  //  TODO
  return true;
}

static void getBeginCmd(WisolContext *context, WisolCmd list[]) {  //  Fetch list of startup commands for Wisol.
  //  Return the list of UART commands to start up the Wisol module.
  int i = 0;
  //  Set emulation mode.
  list[i++] = {
    context->useEmulator  //  If emulator mode,
      ? F(CMD_EMULATOR_ENABLE)  //  Device will only talk to SNEK emulator.
      : F(CMD_EMULATOR_DISABLE),  //  Else device will only talk to Sigfox network.
    1, NULL };
  //  Get Sigfox device ID and PAC.
  list[i++] = { F(CMD_GET_ID), 1, getID };
  list[i++] = { F(CMD_GET_PAC), 1, getPAC };
  list[i++] = endOfList;
  // list[i++] = WisolCmd(NULL, 0, NULL);  //  End of list.
  context->cmdList = list;
  context->cmdIndex = 0;
}

static void convertCmdToUART(
  WisolCmd *cmd,
  WisolContext *context, 
  UARTMsg *uartMsg, 
  Evt_t successEvent0, 
  Evt_t failureEvent0) {
  String sendData(cmd->sendData);
  //  Convert the Wisol command into a UART message.
  const char *strSendData = sendData.c_str();
  // strncpy(uartMsg->sendData, "AT$I=10\r", maxUARTMsgLength);  //  TODO
  strncpy(uartMsg->sendData, strSendData, maxUARTMsgLength);  //  Copy the command string.
  strncat(uartMsg->sendData, CMD_END, maxUARTMsgLength);  //  Terminate the command with "\r".
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
  context->device = "";
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
