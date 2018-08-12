//  Functions to send and receive data from the UART serial port, e.g. for Wisol module.
#include <Arduino.h>
#include <SoftwareSerial.h>
#include "cocoos_cpp.h"
#include "uart.h"

static String toHex(char c);
static void rememberMarker(UARTContext *context);
static void logSendReceive(UARTContext *context);
static void logBuffer(const __FlashStringHelper *prefix, const char *sendData, char markerChar,
                            uint8_t *markerPos, uint8_t markerCount);

//  Use a macro for logging.
#define log1(x) { echoPort->println(x); }
#define log2(x, y) { echoPort->print(x); echoPort->println(y); }
#define log3(x, y, z) { echoPort->print(x); echoPort->print(y); echoPort->println(z); }
#define log4(x, y, z, a) { echoPort->print(x); echoPort->print(y); echoPort->print(z); echoPort->println(a); }

//  Macro for testing the timer accuracy.
#define TEST_TIMER(ms) \
    context = (UARTContext *) task_get_data(); \
    Serial.println(String(F("Expect ")) + String(ms) + String(F(": ")) + String(millis() - context->testTimer)); \
    Serial.flush(); \
    context->testTimer = millis();

static const uint16_t delayAfterStart = 200;
static const uint16_t delayAfterSend = 10;
static const uint16_t delayAfterReceive = 10;

//  Drop all data passed to this port.  Used to suppress echo output.
class NullPort: public Print {
  virtual size_t write(uint8_t) { return 0; }
};

static NullPort nullPort;
//// static uint8_t markers = 0;
static String data;

//  Remember where in response the '>' markers were seen.
const uint8_t markerPosMax = 5;
static uint8_t markerPos[markerPosMax];

SoftwareSerial *serialPort = NULL;  //  Serial port for the SIGFOX module.
Print *echoPort = &Serial;  //  Port for sending echo output.  Defaults to Serial.
Print *lastEchoPort = &Serial;  //  Last port used for sending echo output.
unsigned long lastSend;  //  Timestamp of last send.

void uart_task(void) {
  //  This task loops and waits for an incoming message containing UART data to be sent.
  //  sendData contains a string of ASCII chars to be sent to the UART port.
  //  We send the sendData to the port.  expectedMarkerCount is the number of 
  //  end-of-command markers '\r' we expect to see.  actualMarkerCount 
  //  contains the actual number seen. We trigger to the caller the events successEvent or failureEvent
  //  depending on success/failure sending the data.  Response is recorded in the
  //  "response" variable of the context, for the caller to retrieve.
  Serial.begin(9600);  //  TODO
  UARTContext *context;  //  The context for the task.
  static UARTMsg msg;  //  The received message.
  uint8_t sendChar;  //  Character to be sent.
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.

  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) { //  Run the UART sending code forever. So the task never ends.
    //// debug(F("msg_receive")); ////
    msg_receive(os_get_running_tid(), &msg);
    context = (UARTContext *) task_get_data();  //  Must fetch again after msg_receive().
    log2(F(" - Wisol.sendData: "), context->msg->sendData);  //// log2(F("expectedMarkerCount / timeout: "), String(context->msg->expectedMarkerCount) + String(F(" / ")) + String(context->msg->timeout));
    context->msg = &msg;  //  Remember the message until it's sent via UART.

    //  Initialise the context for the task. These variables will change while sending.
    context->status = true;  //  Assume the return status will be successful.
    context->sendIndex = 0;  //  Index of next char to be sent.
    context->sentTime = 0;  //  Timestamp at which we completed sending.
    context->response[0] = 0;  //  Empty the response buffer.
    context->actualMarkerCount = 0;    

    //  Initialise the UART port.
    serialPort->begin(MODEM_BITS_PER_SECOND);  //  Start the UART interface.
    task_wait(delayAfterStart);  //  Wait for the UART port to be ready.
    context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().    
    serialPort->listen();  //  Start listening for responses from the UART port.
    
    for (;;) {  //  Send the sendData char by char.
      //  If there is no data left to send, continue to the receive step.
      if (context->sendIndex >= strlen(context->msg->sendData)
        || context->sendIndex >= maxUARTMsgLength) { break; }
      //  Send the next char.
      sendChar = (uint8_t) context->msg->sendData[context->sendIndex];
      serialPort->write(sendChar);
      context->sendIndex++;  ////  Serial.println(String(F("send: ")) + String((char) sendChar) + String(F(" / ")) + String(toHex((char)sendChar))); ////
      task_wait(delayAfterSend);  //  Need to wait a while because SoftwareSerial has no FIFO and may overflow.
      context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().
    }
    context->sentTime = millis();  //  Start the timer for detecting receive timeout.

    for (;;) {  //  Read the response.  Loop until timeout or we see the end of response marker.
      //  If receive step has timed out, quit.
      const unsigned long currentTime = millis();
      if (currentTime - context->sentTime > context->msg->timeout) {
        logBuffer(F("<< (Timeout)"), "", context->msg->markerChar, 0, 0);
        break;
      }
      //  No data is available in the serial port sendData to receive now.  We retry later.
      if (serialPort->available() <= 0) { continue; }  ////  TODO task_wait

      //  Attempt to read the data.
      int receiveChar = serialPort->read();  ////  Serial.println(String("receive: ") + String((char) receiveChar) + " / " + String(toHex((char)receiveChar))); ////

      //  No data is available now.  We retry.
      if (receiveChar == -1) { continue; }  ////  TODO task_wait

      if (receiveChar == context->msg->markerChar) {
        //  We see the "\r" marker.
        rememberMarker(context);  //  Remember the marker location so we can format the debug output.
        context->actualMarkerCount++;  //  Count the number of end markers.
        //  If we have encountered all the markers we need, stop receiving.
        if (context->actualMarkerCount >= context->msg->expectedMarkerCount) { break; }
        continue;  //  Else continue to receive next char.
      }
      //  If not "\r" marker, append the received char to the response.
      int len = strlen(context->response);
      if (len < maxUARTMsgLength) {
        context->response[len] = (char) receiveChar;
        context->response[len + 1] = 0;
      }  ////   Serial.println(String(F("response: ")) + context->response); log2(F("receiveChar "), receiveChar);
    }
    serialPort->end();  //  Finished the send/receive.  We close the UART port.
    context = (UARTContext *) task_get_data();  //  Must fetch again to be safe.

    //  If we did not see the expected number of '\r' markers, record the error.
    if (context->actualMarkerCount < context->msg->expectedMarkerCount) { context->status = false; }
    logSendReceive(context);  //  Log the status and actual bytes sent and received.

    if (context->status == true) {
      //  If no error, trigger the success event to caller.
      //  The caller can read the response from the context.response.
      event_signal(context->msg->successEvent);      
    } else {
      //  If we hit an error, trigger the failure event to the caller.
      event_signal(context->msg->failureEvent);  //  Trigger the failure event.
    }
    
    //  Test whether the timer is accurate while multitasking.
    context->testTimer = millis();
    task_wait(10); TEST_TIMER(10);  //  10 milliseconds. First time may not be accurate.
    task_wait(10); TEST_TIMER(10);  //  10 milliseconds
    task_wait(100); TEST_TIMER(100);  //  100 milliseconds
    task_wait(200); TEST_TIMER(200);  //  200 milliseconds
  }  //  Loop back and wait for next queued message.
  task_close();  //  End of the task. Should not come here.
}

#ifdef NOTUSED
bool Wisol::begin() {
  //  Wait for the module to power up, configure transmission frequency.
  //  Return true if module is ready to send.
  lastSend = 0;
  for (int i = 0; i < 5; i++) {
    //  Retry 5 times.
    #ifdef BEAN_BEAN_BEAN_H
        Bean.sleep(7000);  //  For Bean, delay longer to allow Bluetooth debug console to connect.
    #else  // BEAN_BEAN_BEAN_H
        delay(2000);
    #endif // BEAN_BEAN_BEAN_H
    String result;
    if (useEmulator) {
      //  Emulation mode.
      if (!enableEmulator(result)) continue;
    } else {
      //  Disable emulation mode.
      if (!disableEmulator(result)) continue;
    }
    //  TODO: Check whether emulator is used for transmission.
    //  log1(F(" - Checking emulation mode (expecting 0)...")); int emulator = 0;
    //  if (!getEmulator(emulator)) continue;

    //  Read SIGFOX ID and PAC from module.
    log1(F(" - Getting SIGFOX ID..."));  String id, pac;
    if (!getID(id, pac)) continue;
    echoPort->print(F(" - SIGFOX ID = "));  Serial.println(id);
    echoPort->print(F(" - PAC = "));  Serial.println(pac);

    //  Set the frequency of SIGFOX module.
    // log1(F(" - Setting frequency for country "));
    // echoPort->write((uint8_t) (country / 8));
    // echoPort->write((uint8_t) (country % 8));
    if (country == COUNTRY_JP) {  //  Set Japan frequency (RCZ3).
      if (!setFrequencyJP(result)) continue;
    } else if (country == COUNTRY_US) {  //  Set US frequency (RCZ2).
      if (!setFrequencyUS(result)) continue;
    } else if (
      country == COUNTRY_FR
      || country == COUNTRY_OM
      || country == COUNTRY_SA) {  //  Set France frequency (RCZ1).
      if (!setFrequencyETSI(result)) continue;
    } else { //  Rest of the world runs on RCZ4.
      if (!setFrequencySG(result)) continue;
    }
    log2(F(" - Set frequency result = "), result);

    //  Get and display the frequency used by the SIGFOX module.  Should return 3 for RCZ4 (SG/TW).
    log1(F(" - Getting frequency (expecting 3)..."));  String frequency;
    if (!getFrequency(frequency)) continue;
    log2(F(" - Frequency (expecting 3) = "), frequency);
    return true;  //  Init module succeeded.
  }
  return false;  //  Failed to init module.
}
#endif

void setup_uart(UARTContext *context, uint8_t rx, uint8_t tx, bool echo) {
  //  Init the module with the specified transmit and receive pins.
  //  Default to no echo.
  serialPort = new SoftwareSerial(rx, tx);
  if (echo) echoPort = &Serial;
  else echoPort = &nullPort;
  lastEchoPort = &Serial;
}

static void rememberMarker(UARTContext *context) {
  //  When we see the "\r" marker: Remember the marker location so we can format the debug output.
  if (context->actualMarkerCount < markerPosMax) {
    markerPos[context->actualMarkerCount] = strlen(context->response); 
  }
}

static void logSendReceive(UARTContext *context) {
  //  Log the status and actual bytes sent and received.
  //  log2(F(">> "), echoSend); if (echoReceive.length() > 0) { log2(F("<< "), echoReceive); }
  logBuffer(F(">> "), context->msg->sendData, context->msg->markerChar, 0, 0);
  logBuffer(F("<< "), context->response, context->msg->markerChar, markerPos, context->actualMarkerCount);
  Serial.print(F("<< status: "));
  Serial.println(context->status);
  Serial.print(F("<< response: "));
  Serial.println(context->response);
  Serial.print(F("<< actualMarkerCount: "));
  Serial.println(context->actualMarkerCount);

  if (context->status == true) { log2(F(" - Wisol.sendData: response: "), context->response); }
  else if (strlen(context->response) == 0) { log1(F(" - Wisol.sendData: Error: Response timeout")); }
  else { log2(F(" - Wisol.sendData: Error: Unknown response: "), context->response); }
  Serial.flush();
}

//  Convert nibble to hex digit.
static const char nibbleToHex[] = "0123456789abcdef";

static void logBuffer(const __FlashStringHelper *prefix, const char *sendData, char markerChar,
                            uint8_t *markerPos, uint8_t markerCount) {
  //  Log the send/receive sendData for debugging.  markerPos is an array of positions in sendData
  //  where the '>' marker was seen and removed.
  echoPort->print(prefix);
  size_t m = 0, i = 0;
  for (i = 0; i < strlen(sendData); i = i + 2) {
    if (m < markerCount && markerPos[m] == i) {
      echoPort->print("0x");
      echoPort->write((uint8_t) nibbleToHex[markerChar / 16]);
      echoPort->write((uint8_t) nibbleToHex[markerChar % 16]);
      m++;
    }
    echoPort->write((uint8_t) sendData[i]);
    echoPort->write((uint8_t) sendData[i + 1]);
  }
  if (m < markerCount && markerPos[m] == i) {
    echoPort->print("0x");
    echoPort->write((uint8_t) nibbleToHex[markerChar / 16]);
    echoPort->write((uint8_t) nibbleToHex[markerChar % 16]);
    m++;
  }
  echoPort->write('\n');
}

static String toHex(char c) {
  //  Convert the char to a string of 2 hex digits.
  byte *b = (byte *) &c;
  String bytes;
  if (b[0] <= 0xF) bytes.concat('0');
  bytes.concat(String(b[0], 16));
  return bytes;
}
