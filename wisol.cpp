//  Functions to send and receive messages to/from Sigfox network via Wisol module.
#include <Arduino.h>
#include "cocoos_cpp.h"
#include "uart.h"
#include "wisol.h"

#define END_OF_RESPONSE '\r'  //  Character '\r' marks the end of response.
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
#define CMD_END "\r"
#define CMD_RCZ1 "AT$IF=868130000"  //  EU / RCZ1 Frequency
#define CMD_RCZ2 "AT$IF=902200000"  //  US / RCZ2 Frequency
#define CMD_RCZ3 "AT$IF=902080000"  //  JP / RCZ3 Frequency
#define CMD_RCZ4 "AT$IF=920800000"  //  RCZ4 Frequency
#define CMD_MODULATION_ON "AT$CB=-1,1"  //  Modulation wave on.
#define CMD_MODULATION_OFF "AT$CB=-1,0"  //  Modulation wave off.
#define CMD_EMULATOR_DISABLE "ATS410=0"  //  Device will only talk to Sigfox network.
#define CMD_EMULATOR_ENABLE "ATS410=1"  //  Device will only talk to SNEK emulator.

void wisol_task(void) {
  Serial.begin(9600);  //  TODO

  WisolContext *context;
  static WisolMsg msg;  //  TODO
  static UARTMsg uartMsg;  //  TODO
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.

  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) { //  Run the data sending code forever. So the task never ends.
    //  TODO: On task startup, send the UART commands to initialise the Wisol module.
    context = (WisolContext *) task_get_data();
    if (context->firstTime) {
      context->firstTime = false;
      strncpy(uartMsg.sendData, "AT$I=10\r", maxUARTMsgLength);  //  TODO
      uartMsg.timeout = 1000;  //  TODO: COMMAND_TIMEOUT
      uartMsg.markerChar = '\r';  //  TODO: END_OF_RESPONSE
      uartMsg.expectedMarkerCount = 1;  //  TODO
      uartMsg.successEvent = event_create();  //  TODO
      uartMsg.failureEvent = event_create();  //  TODO
      msg_post(context->uartTaskID, uartMsg);  //  Send the message to the UART task for transmission.
    }

    //  Wait for an incoming message containing sensor data to be transmitted.
    //// debug(F("msg_receive")); ////
    msg_receive(os_get_running_tid(), &msg);
    context = (WisolContext *) task_get_data();  //  Must fetch again after msg_receive().
    context->msg = &msg;  //  Remember the message until it's sent via UART.

    //  TODO: Check whether we should transmit.
  }
  task_close();  //  End of the task.
}

void setup_wisol(
  WisolContext *context, 
  int8_t uartTaskID, 
  Country country0, 
  bool useEmulator0) {
  //  Init the Wisol module.
  context->uartTaskID = uartTaskID;
  context->zone = 4;  //  RCZ4
  context->country = country0;
  context->useEmulator = useEmulator0;
  context->device = "";
  context->firstTime = true;
}
