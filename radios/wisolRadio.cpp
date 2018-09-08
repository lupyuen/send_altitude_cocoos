#include "cocoos.h"
#include "wisolRadio.h"

#include "network.h"
#include "string.h"
#include "utils.h"

static bool getID(NetworkContext *context, const char *response);
static bool getPAC(NetworkContext *context, const char *response);
static bool getDownlink(NetworkContext *context, const char *response0);
static bool checkChannel(NetworkContext *context, const char *response);

#define MAX_OUTPUT_SIZE N_SENSORS * SENSOR_DATA_SIZE * 4 * 2 + 1
static char output[MAX_OUTPUT_SIZE];

WisolRadio::WisolRadio(UartSerial *serial):
  dev(serial),
  writepos(0),
  readpos(0),
  rxDoneEvt(NO_EVENT),
  expectedMarkerCount(0),
  receivedMarkers(0),
  nCommands(0),
  sending(false) {
  dev->registerReader(this);
}

bool WisolRadio::send(const uint8_t *data, uint8_t len) {
  sending = true;
  return dev->write(data, len);
}

uint8_t WisolRadio::receive(uint8_t *buf) {

    if (receivedMarkers == expectedMarkerCount) {
        uint8_t cnt = 0;
        uint8_t *data = &rxbuf[0];
        uint8_t nbytes = 0;

        while(cnt < expectedMarkerCount) {
            *buf++ = *data;
            nbytes++;
            if (*data == '\r') {
                cnt++;
            }
            data++;
        }
        receivedMarkers = 0;
        return nbytes;
    }
    return 0;
}

void WisolRadio::setDoneEvent(Evt_t event) {
  rxDoneEvt = event;
}

void WisolRadio::setMarkerCount(unsigned count) {
    expectedMarkerCount = count;
}

void WisolRadio::update(uint8_t data) {
  if (sending) {
    rxbuf[writepos] = data;

    if ((data == '\r') && (++receivedMarkers == expectedMarkerCount)){

      sending = false;

      // we have received a full message, signal upper layer
      if (NO_EVENT != rxDoneEvt) {
        event_ISR_signal(rxDoneEvt);
      }
      // We just hope that the message is read before new data arrives
      // TODO: put received data in a thread safe circular buffer
      writepos = 0;
    }
    else {
      writepos = (writepos+1) % 128;
    }
  }
}

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

//unsigned WisolRadio::getStepBegin(NetworkCmd list[], int listSize, bool useEmulator) {
//  //  Return the list of Wisol AT commands for the Begin Step, to start up the Wisol module.
//  nCommands = 0;
//
//  addCmd(list, listSize, { useEmulator ? F(CMD_EMULATOR_ENABLE) : F(CMD_EMULATOR_DISABLE), 1, NULL, NULL, NULL });
//
//  //  Get Sigfox device ID and PAC.
//  addCmd(list, listSize, { F(CMD_GET_ID), 1, getID, NULL, NULL });
//  addCmd(list, listSize, { F(CMD_GET_PAC), 1, getPAC, NULL, NULL });
//  return nCommands;
//}

unsigned WisolRadio::getStepBegin(NetworkCmd list[], int listSize, bool useEmulator) {
  //  Return the list of Wisol AT commands for the Begin Step, to start up the Wisol module.
  nCommands = 0;

  addCmd(list, listSize, {useEmulator ? F(CMD_EMULATOR_ENABLE) : F(CMD_EMULATOR_DISABLE), NULL, NULL, NULL, 1});

  //  Get Sigfox device ID and PAC.
  addCmd(list, listSize, {F(CMD_GET_ID), getID, NULL, NULL, 1 });
  addCmd(list, listSize, {F(CMD_GET_PAC), getPAC, NULL, NULL, 1});
  return nCommands;
}

void WisolRadio::addCmd(NetworkCmd list[], int listSize, NetworkCmd cmd) {
  //  Append the UART message to the command list.
  //  Stop if we have overflowed the list.
  int i = getCmdIndex(list, listSize);
  list[i++] = cmd;
  list[i++] = endOfList;
  nCommands++;
}

int WisolRadio::getCmdIndex(NetworkCmd list[], int listSize) {
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
    i = listSize - 2;
    if (i < 0) i = 0;
  }
  return i;
}

unsigned WisolRadio::getStepSend(NetworkContext *context, NetworkCmd list[], int listSize, const float payload[], bool enableDownlink) {

  //  Return the list of Wisol AT commands for the Send Step, to send the payload.
  //  Payload contains a string of hex digits, up to 24 digits / 12 bytes.
  //  We prefix with AT$SF= and send to the transceiver.  If enableDownlink is true, we append the
  //  CMD_SEND_MESSAGE_RESPONSE command to indicate that we expect a downlink repsonse.
  //  The downlink response message from Sigfox will be returned in the response parameter.
  //  Warning: This may take up to 1 min to run.
  //  debug(F(" - wisol.getStepSend")); ////

  nCommands = 0;

  //  Set the output power for the zone.
  getStepPowerChannel(context, list, listSize);

  //  Compose the payload sending command.
  uint8_t markers = 1;  //  Wait for 1 line of response.
  bool (*processFunc)(NetworkContext *context, const char *response) = NULL;  //  Function to process result.
  const __FlashStringHelper *sendData2 = NULL;  //  Text to be appended to payload.

  // If no downlink: Send CMD_SEND_MESSAGE + payload
  if (enableDownlink) {
    //  For downlink mode: send CMD_SEND_MESSAGE + payload + CMD_SEND_MESSAGE_RESPONSE
    markers++;  //  Wait for one more response line.
    processFunc = getDownlink;  //  Process the downlink message.
    sendData2 = F(CMD_SEND_MESSAGE_RESPONSE);  //  Append suffix to payload.
  }

  // convert each float in the payload to a string
  NetworkCmd cmd;
  char *out = &output[0];

  for (int i = 0; i < N_SENSORS; i++) {
    for (int j = 0; j < SENSOR_DATA_SIZE; j++) {
      uint32_t tmp = payload[i*N_SENSORS + j] * 10000000;
      uint32ToStringBase16(out, tmp);
      out += 8;
    }
  }

  *out = '\0';

  addCmd(list, listSize, {CMD_SEND_MESSAGE, processFunc, output, sendData2, markers});
  return nCommands;
}

void WisolRadio::getStepPowerChannel(NetworkContext *context, NetworkCmd list[], int listSize) {
  //  Return the Wisol AT commands to set the transceiver output power and channel for the zone.
  //  See WISOLUserManual_EVBSFM10RxAT_Rev.9_180115.pdf, http://kochingchang.blogspot.com/2018/06/minisigfox.html
  //  debug(F(" - wisol.getStepPowerChannel")); ////
  NetworkCmd cmd;
  switch(context->zone) {
    case RCZ1:
    case RCZ3:
      //  Set the transceiver output power.

      addCmd(list, listSize, {F(CMD_OUTPUT_POWER_MAX), NULL, NULL, NULL, 1});
      break;
    case RCZ2:
    case RCZ4: {
      //  Get the current and next macro channel usage. Returns X,Y:
      //  X: boolean value, indicating previous TX macro channel was in the Sigfox default channel
      //  Y: number of micro channel available for next TX request in current macro channel.
      //  Call checkChannel() to check the response.
        addCmd(list, listSize, {F(CMD_GET_CHANNEL), checkChannel, NULL, NULL, 1});

      //  If X=0 or Y<3, send CMD_RESET_CHANNEL to reset the device on the default Sigfox macro channel.
      //  Note: Don't use with a duty cycle less than 20 seconds.
      //  Note: checkChannel() will change this command to CMD_NONE if not required.
      addCmd(list, listSize, {F(CMD_RESET_CHANNEL), NULL, NULL, NULL,1});
      break;
    }
  }
}

static bool getDownlink(NetworkContext *context, const char *response0) {
  //  Extract the downlink message and write into the context response.
  //  context response will be returned as an 8-byte hex string, e.g. "0123456789ABCDEF"
  //  or a timeout error after 1 min e.g. "ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT"

  //  Get a writeable response pointer in the uartContext.
  char *response = context->response;

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
    strncat(response, after, MAX_RADIO_SEND_MSG_SIZE - strlen(response));
    response[MAX_RADIO_SEND_MSG_SIZE] = 0;  //  Terminate the response in case of overflow.
  } else {
    //  Return error e.g. ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
    context->status = false;
  }
  //  Remove all spaces.
  int src = 0, dst = 0;
  for (;;) {
    if (src >= MAX_RADIO_SEND_MSG_SIZE) break;
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
  response[MAX_RADIO_SEND_MSG_SIZE] = 0;  //  Terminate the response in case of overflow.
  context->downlinkData = response;
  return true;
}
///////////////////////////////////////////////////////////////////////////////
//  Wisol Response Processing Functions: Called to process response when response
//  is received from Wisol AT Command.

static bool getID(NetworkContext *context, const char *response) {
  //  Save the device ID to context.
  strncpy(context->device, response, MAX_DEVICE_ID_SIZE);
  context->device[MAX_DEVICE_ID_SIZE] = 0;  //  Terminate the device ID in case of overflow.
  return true;
}

static bool getPAC(NetworkContext *context, const char *response) {
  //  Save the PAC code to context.  Note that the PAC is only valid
  //  for the first registration in the Sigfox portal.  After
  //  registering the device, the PAC is changed in the Sigfox portal
  //  but not in the Wisol AT Command.  You must get the updated
  //  PAC from the Sigfox portal if you wish to transfer the device.
  strncpy(context->pac, response, MAX_DEVICE_CODE_SIZE);
  context->pac[MAX_DEVICE_CODE_SIZE] = 0;  //  Terminate the PAC code in case of overflow.
  return true;
}

static bool checkChannel(NetworkContext *context, const char *response) {
  //  Parse the CMD_GET_CHANNEL response "X,Y" to determine if we need to send the CMD_RESET_CHANNEL command.
  //  If not needed, change the next command to CMD_NONE.

  //  CMD_GET_CHANNEL gets the current and next macro channel usage. Returns X,Y:
  //  X: boolean value, indicating previous TX macro channel was in the Sigfox default channel
  //  Y: number of micro channel available for next TX request in current macro channel.

  //  If X=0 or Y<3, send CMD_RESET_CHANNEL to reset the device on the default Sigfox macro channel.
  //  Note: Don't use with a duty cycle less than 20 seconds.
  //  debug(F("checkChannel: "), response);
  if (strlen(response) < 3) {  //  If too short, return error.
    return false;  //  Failure
  }
  //  Change chars to numbers.
  int x = response[0] - '0';
  int y = response[2] - '0';
  if (x != 0 && y >= 3) {
    //  No need to reset channel. We change CMD_RESET_CHANNEL to CMD_NONE.
    //  debug(F(" - wisol.checkChannel: Continue channel"));
    if (context->nCommands >= MAX_NETWORK_CMD_LIST_SIZE) {
      return false;  //  Failure
    }
    if (context->cmd->sendData == NULL) {
      return false;  //  Failure
    }
    context->cmd->sendData = F(CMD_NONE);
  } else {
    //  Continue to send CMD_RESET_CHANNEL
    //  debug(F(" - wisol.checkChannel: Reset channel"));
  }
  return true;  //  Success
}



