//  Functions to send and receive data from the UART serial port, e.g. for Wisol module.
#include <Arduino.h>
#include <SoftwareSerial.h>
#include "cocoos_cpp.h"
#include "uart.h"

static String toHex(char c);
static void logBuffer(const __FlashStringHelper *prefix, const char *buffer, char markerChar,
                            uint8_t *markerPos, uint8_t markerCount);

//  Use a macro for logging.
#define log1(x) { echoPort->println(x); }
#define log2(x, y) { echoPort->print(x); echoPort->println(y); }
#define log3(x, y, z) { echoPort->print(x); echoPort->print(y); echoPort->println(z); }
#define log4(x, y, z, a) { echoPort->print(x); echoPort->print(y); echoPort->print(z); echoPort->println(a); }

static const uint16_t delayAfterStart = 200;
static const uint16_t delayAfterSend = 10;
static const uint16_t delayAfterReceive = 10;

//  Drop all data passed to this port.  Used to suppress echo output.
class NullPort: public Print {
  virtual size_t write(uint8_t) {}
};

static NullPort nullPort;
static uint8_t markers = 0;
static String data;

//  Remember where in response the '>' markers were seen.
const uint8_t markerPosMax = 5;
static uint8_t markerPos[markerPosMax];

SoftwareSerial *serialPort = NULL;  //  Serial port for the SIGFOX module.
Print *echoPort = &Serial;  //  Port for sending echo output.  Defaults to Serial.
Print *lastEchoPort = &Serial;  //  Last port used for sending echo output.
unsigned long lastSend;  //  Timestamp of last send.

void uart_task(void) {
  Serial.begin(9600);  //  TODO

  //  buffer contains a string of ASCII chars to be sent to the transceiver.
  //  We send the buffer to the transceiver.  Return true if successful.
  //  expectedMarkerCount is the number of end-of-command markers '\r' we
  //  expect to see.  actualMarkerCount contains the actual number seen.

  //  This function runs in normal sequential mode (like a normal function)
  //  as well as Finite State Machine mode.  In State Machine mode,
  //  we run each step of this function as a separate function call,
  //  as controlled by the Finite State Machine and the step paramter.
  //  The Finite State Machine sets the step parameter to a non-zero value
  //  to indicate the step to jump to.

  UARTContext *context;
  UARTMsg msg;
  uint8_t sendChar;
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.

  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) { //  Run the UART sending code forever. So the task never ends.
    //  Wait for an incoming message containing UART data to be sent.
    //// debug(F("msg_receive")); ////
    msg_receive(os_get_running_tid(), &msg);
    context = (UARTContext *) task_get_data();  //  Must fetch again after msg_receive().
    context->msg = &msg;  //  Remember the message until it's sent via UART.

#define TEST_TIMER
#ifdef TEST_TIMER
    //  Test whether the timer is accurate while multitasking.
    context->testTimer = millis();
    task_wait(10); context = (UARTContext *) task_get_data();
    Serial.println(String(F("Expect 10: ")) + String(millis() - context->testTimer));

    context->testTimer = millis();
    task_wait(100); context = (UARTContext *) task_get_data();
    Serial.println(String(F("Expect 100: ")) + String(millis() - context->testTimer));

    context->testTimer = millis();
    task_wait(200); context = (UARTContext *) task_get_data();
    Serial.println(String(F("Expect 200: ")) + String(millis() - context->testTimer));
#endif // TEST_TIMER

    log2(F(" - Wisol.sendBuffer: "), context->msg->buffer);
    //// 
    log2(F("expectedMarkerCount / timeout: "), String(context->msg->expectedMarkerCount) + String(F(" / ")) + String(context->msg->timeout));

    //  Initialise context.
    context->status = false;  //  Return status.
    context->sendIndex = 0;  //  Index of next char to be sent.
    context->sentTime = 0;  //  Timestamp at which we completed sending.
    context->response = "";
    context->actualMarkerCount = 0;

    //  Start the serial interface for the transceiver.
    serialPort->begin(MODEM_BITS_PER_SECOND);
    task_wait(delayAfterStart);
    context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().

    //  Start listening for responses from the transceiver.
    serialPort->listen();

    //  Send the buffer char by char.
    for (;;) {
      //  If there is no data left to send, continue to next step.
      if (context->sendIndex >= strlen(context->msg->buffer)
        || context->sendIndex >= maxUARTMsgLength) { break; }

      //  Send the next char.
      sendChar = (uint8_t) context->msg->buffer[context->sendIndex];
      serialPort->write(sendChar);
      context->sendIndex++;
      ////  
      Serial.println(String(F("send: ")) + String((char) sendChar) + String(F(" / ")) + String(toHex((char)sendChar))); ////
      task_wait(delayAfterSend);  //  Need to wait a while because SoftwareSerial has no FIFO and may overflow.
      context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().
    }
    context->sentTime = millis();  //  Start the timer for detecting receive timeout.
    
    //  Read response.  Loop until timeout or we see the end of response marker.
    for (;;) {
      //  If receive step has timed out, quit.
      const unsigned long currentTime = millis();
      if (currentTime - context->sentTime > context->msg->timeout) {
        logBuffer(F("<< (Timeout)"), "", context->msg->markerChar, 0, 0);
        break;
      }
      //  No data is available in the serial port buffer to receive now.  We retry later.
      if (serialPort->available() <= 0) { continue; }  ////  TODO task_wait

      //  Attempt to read the data.
      int receiveChar = serialPort->read();
      ////  Serial.println(String("receive: ") + String((char) receiveChar) + " / " + String(toHex((char)receiveChar))); ////

        //  No data is available now.  We retry.
      if (receiveChar == -1) { continue; }  ////  TODO task_wait

      if (receiveChar == context->msg->markerChar) {
        //  We see the "\r" marker. Remember the marker location so we can format the debug output.
        if (context->actualMarkerCount < markerPosMax) {
          markerPos[context->actualMarkerCount] = context->response.length(); 
        }
        context->actualMarkerCount++;  //  Count the number of end markers.

        //  We have encountered all the markers we need.  Stop receiving.
        if (context->actualMarkerCount >= context->msg->expectedMarkerCount) break;
        continue;  //  Continue to receive next char.
      }

      //  Else append the received char to the response.
      context->response.concat(String((char) receiveChar));
      ////  Serial.println(String("response: ") + context->response); ////
      ////  log2(F("receiveChar "), receiveChar);
    }
    //  Finished the send and receive.  We close the serial port.
    //  In case of timeout, also close the serial port.
    serialPort->end();

    //  Log the actual bytes sent and received.
    //  log2(F(">> "), echoSend); if (echoReceive.length() > 0) { log2(F("<< "), echoReceive); }
    logBuffer(F(">> "), context->msg->buffer, context->msg->markerChar, 0, 0);
    logBuffer(F("<< "), context->response.c_str(), context->msg->markerChar, markerPos, context->actualMarkerCount);

    //  If we did not see the expected number of '\r', something is wrong.
    if (context->actualMarkerCount < context->msg->expectedMarkerCount) {
      context->status = false;  //  Return failure.
      if (context->response.length() == 0) {
        log1(F(" - Wisol.sendBuffer: Error: No response"));  //  Response timeout.
      } else {
        log2(F(" - Wisol.sendBuffer: Error: Unknown response: "), context->response);
      }
    } else {
      context->status = true;  //  Return success.
      log2(F(" - Wisol.sendBuffer: response: "), context->response);
    }

    ////
    context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().
    Serial.print(F("status = "));
    Serial.println(context->status);
    Serial.print(F("response = "));
    Serial.println(context->response);
    Serial.print(F("actualMarkerCount = "));
    Serial.println(context->actualMarkerCount);
    ////

    //  Return the event to the caller.
    if (context->status == true) { event_signal(context->msg->successEvent); }
    else { event_signal(context->msg->failureEvent); }
  }

  task_close();  //  End of the task.
}

void setup_uart(UARTContext *context, uint8_t rx, uint8_t tx, bool echo) {
  //  Init the module with the specified transmit and receive pins.
  //  Default to no echo.
  serialPort = new SoftwareSerial(rx, tx);
  if (echo) echoPort = &Serial;
  else echoPort = &nullPort;
  lastEchoPort = &Serial;
}

//  Convert nibble to hex digit.
static const char nibbleToHex[] = "0123456789abcdef";

static void logBuffer(const __FlashStringHelper *prefix, const char *buffer, char markerChar,
                            uint8_t *markerPos, uint8_t markerCount) {
  //  Log the send/receive buffer for debugging.  markerPos is an array of positions in buffer
  //  where the '>' marker was seen and removed.
  echoPort->print(prefix);
  int m = 0, i = 0;
  for (i = 0; i < strlen(buffer); i = i + 2) {
    if (m < markerCount && markerPos[m] == i) {
      echoPort->print("0x");
      echoPort->write((uint8_t) nibbleToHex[markerChar / 16]);
      echoPort->write((uint8_t) nibbleToHex[markerChar % 16]);
      m++;
    }
    echoPort->write((uint8_t) buffer[i]);
    echoPort->write((uint8_t) buffer[i + 1]);
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
