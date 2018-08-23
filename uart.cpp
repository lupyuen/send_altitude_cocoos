//  Functions to send and receive data from the UART serial port, e.g. for Wisol module.
#include "platform.h"
#include <string.h>
#ifdef ARDUINO
#include <SoftwareSerial.h>
#else
#include <uartint.h>  //  For UARTInterface
#endif  //  ARDUINO
#include <cocoos.h>
#include "display.h"
#include "sensor.h"
#include "uart.h"

static void rememberMarker(UARTContext *context);
static void logChar(char ch);
static void logSendReceive(UARTContext *context);
static void logBuffer(const __FlashStringHelper *prefix, const char *sendData, char markerChar,
                            uint8_t *markerPos, uint8_t markerCount);

#ifdef DISABLE_DEBUG_LOG  //  Don't do debug logging.
#define log1(x) { }
#define log2(x, y) { }
#define log3(x, y, z) { }
#define log4(x, y, z, a) { }

#else  //  Use a macro for logging.

#define log1(x) { debug_println(x); debug_flush(); }
#define log2(x, y) { debug_print(x); debug_println(y); debug_flush(); }
#define log3(x, y, z) { debug_print(x); debug_print(y); debug_println(z); debug_flush(); }
#define log4(x, y, z, a) { debug_print(x); debug_print(y); debug_print(z); debug_println(a); debug_flush(); }
#endif  //  DISABLE_DEBUG_LOG

//  Macro for testing the timer accuracy.
#define TEST_TIMER(ms) \
    context = (UARTContext *) task_get_data(); \
    Serial.println(String(F("Expect ")) + String(ms) + String(F(": ")) + String(millis() - context->testTimer)); \
    Serial.flush(); \
    context->testTimer = millis();

//  Delays in milliseconds.
static const uint16_t delayAfterStart = 200;  //  Delay after UART port init.
static const uint16_t delayAfterSend = 10;  //  Delay after sending data.
static const uint16_t delayReceive = 1000;  //  Delay while receiving data.

//  Remember where in response the '\r' markers were seen.
const uint8_t markerPosMax = 5;
static uint8_t markerPos[markerPosMax];

#ifdef ARDUINO
SoftwareSerial serialPort(UART_RX_PORT, UART_TX_PORT);  //  Serial port for send/receive.
#else
UARTInterface serialPort(UART_RX_PORT, UART_TX_PORT);  //  UART port for send/receive.
#endif  //  ARDUINO

void uart_task(void) {
  //  This task loops and waits for an incoming message containing UART data to be sent.
  //  sendData contains a string of ASCII chars to be sent to the UART port.
  //  We send the sendData to the port.  expectedMarkerCount is the number of 
  //  end-of-command markers '\r' we expect to see.  actualMarkerCount 
  //  contains the actual number seen. We trigger to the caller the events successEvent or failureEvent
  //  depending on success/failure sending the data.  Response is recorded in the
  //  "response" variable of the context, for the caller to retrieve.
  UARTContext *context;  //  The context for the task.
  static UARTMsg msg;  //  The received message.
  uint8_t sendChar;  //  Character to be sent.

  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) { //  Run the UART sending code forever. So the task never ends.
    msg_receive(os_get_running_tid(), &msg);
    context = (UARTContext *) task_get_data();  //  Must fetch again after msg_receive().
    context->msg = &msg;  //  Remember the message until it's sent via UART.
    logBuffer(F(">> "), context->msg->sendData, context->msg->markerChar, 0, 0);
    //  log2(F(" - uart.sendData: "), context->msg->sendData);  //// log2(F("expectedMarkerCount / timeout: "), String(context->msg->expectedMarkerCount) + String(F(" / ")) + String(context->msg->timeout));

    //  Initialise the context for the task. These variables will change while sending.
    context->status = true;  //  Assume the return status will be successful.
    context->sendIndex = 0;  //  Index of next char to be sent.
    context->sentTime = 0;  //  Timestamp at which we completed sending.
    context->response[0] = 0;  //  Empty the response buffer.
    context->actualMarkerCount = 0;  //  How many markers we actually received.

    //  Initialise the UART port.
    serialPort.begin(UART_BITS_PER_SECOND);  //  Start the UART interface.
    task_wait(delayAfterStart);  //  Wait for the UART port to be ready.
    context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().    
    serialPort.listen();  //  Start listening for responses from the UART port.
    
    //  Send Loop
    for (;;) {  //  Send the sendData char by char.
      //  If there is no data left to send, continue to the receive step.
      if (context->sendIndex >= strlen(context->msg->sendData)
        || context->sendIndex >= MAX_UART_SEND_MSG_SIZE) { break; }
      //  Send the next char.
      sendChar = (uint8_t) context->msg->sendData[context->sendIndex];
      serialPort.write(sendChar);
      context->sendIndex++;  ////  debug_println(String(F("send: ")) + String((char) sendChar) + String(F(" / ")) + String(toHex((char)sendChar))); ////
      task_wait(delayAfterSend);  //  Need to wait a while because SoftwareSerial has no FIFO and may overflow.
      context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().
    }
    context->sentTime = millis();  //  Start the timer for detecting receive timeout.

    //  Receive Loop
    for (;;) {  //  Read the response.  Loop until timeout or we see the end of response marker.
      context = (UARTContext *) task_get_data();  //  Must fetch again after task_wait().    
      unsigned long currentTime, elapsedTime, remainingTime;
      currentTime = millis();
      elapsedTime = currentTime - context->sentTime;
      if (elapsedTime > context->msg->timeout) {
        //  If receive step has timed out, quit.
        logBuffer(F("<< (Timeout)"), "", context->msg->markerChar, 0, 0);
        break;
      }
      if (serialPort.available() <= 0) { 
        //  No data is available in the serial port sendData to receive now.  We retry later.
        //  Wait a while before checking receive.
        remainingTime = context->msg->timeout - elapsedTime;
        if (remainingTime > delayReceive) {  //  Wait only if there is sufficient time remaining.
          task_wait(delayReceive); 
        }
        continue;  //  Check again.
      }
      //  Attempt to read the data.
      int receiveChar = serialPort.read();  ////  debug_println(String("receive: ") + String((char) receiveChar) + " / " + String(toHex((char)receiveChar))); ////

      //  No data is available now.  We retry.
      if (receiveChar == -1) { continue; }  //  Should not come here.

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
      if (len >= MAX_UART_RESPONSE_MSG_SIZE) {
        debug_print(F("***** Error: UART response overflow - ")); debug_println(len);
      } else {
        context->response[len] = (char) receiveChar;
        context->response[len + 1] = 0;
      }  ////   debug_println(String(F("response: ")) + context->response); log2(F("receiveChar "), receiveChar);
    }  //  Loop until receive is complete or timeout.
    serialPort.end();  //  Finished the send/receive.  We close the UART port.
    context = (UARTContext *) task_get_data();  //  Must fetch again to be safe.

    //  If we did not see the expected number of '\r' markers, record the error.
    if (context->actualMarkerCount < context->msg->expectedMarkerCount) { context->status = false; }
    logSendReceive(context);  //  Log the status and actual bytes sent and received.

    if (context->msg->responseMsg != NULL) {
      //  If caller has requested for response message, then send it instead of event.
      msg_post(context->msg->responseTaskID, *(context->msg->responseMsg));
      context = (UARTContext *) task_get_data();  //  Must fetch again in case msg_post() blocks.
    } else if (context->status == true) {
      //  If no error, trigger the success event to caller.
      //  The caller can read the response from the context.response.
      event_signal(context->msg->successEvent);      
    } else {
      //  If we hit an error, trigger the failure event to the caller.
      event_signal(context->msg->failureEvent);  //  Trigger the failure event.
    }
    #ifdef NOTUSED    
    //  Test whether the timer is accurate while multitasking.
    context->testTimer = millis();
    task_wait(10); TEST_TIMER(10);  //  10 milliseconds. First time may not be accurate.
    task_wait(10); TEST_TIMER(10);  //  10 milliseconds
    task_wait(100); TEST_TIMER(100);  //  100 milliseconds
    task_wait(200); TEST_TIMER(200);  //  200 milliseconds
    #endif  //  NOTUSED
  }  //  Loop back and wait for next queued message.
  task_close();  //  End of the task. Should not come here.
}

void setup_uart(
  UARTContext *context,  //  Will be used to store the context of the UART Task.
  char *response) {      //  Buffer that will be used to store the UART response.
  //  Init the UART context with the response buffer.
  context->response = response;
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
  //  logBuffer(F(">> "), context->msg->sendData, context->msg->markerChar, 0, 0);
  logBuffer(F("<< "), context->response, context->msg->markerChar, markerPos, context->actualMarkerCount);

  // debug_print(F("<< status: ")); debug_println(context->status);
  // debug_print(F("<< response: ")); debug_println(context->response);
  // debug_print(F("<< actualMarkerCount: ")); debug_println(context->actualMarkerCount);

  if (context->status == true) { /* log2(F(" - uart.sendData: response: "), context->response); */ }
  else if (strlen(context->response) == 0) { log1(F("***** uart.sendData: Error: Response timeout")); }
  else { log2(F("***** uart.sendData: Error: Unknown response: "), context->response); }
  debug_flush();
}

//  Convert nibble to hex digit.
static const char nibbleToHex[] = "0123456789abcdef";

static void logBuffer(const __FlashStringHelper *prefix, const char *data, char markerChar,
                            uint8_t *markerPos, uint8_t markerCount) {
  //  Log the send/receive data for debugging.  markerPos is an array of positions in data
  //  where the '\r' marker was seen and removed.  If markerCount=0, don't show markers.
  debug_print(prefix);
  size_t m = 0, i = 0;
  for (i = 0; i < strlen(data); i++) {
    if (m < markerCount && markerPos[m] == i) {
      logChar(markerChar);
      m++;
    }
    char ch = data[i];
    if (ch < ' ' || ch > '~') { logChar(ch); }  //  Log non-ASCII char in hex.
    else { debug_write((uint8_t) ch); }
  }
  if (m < markerCount && markerPos[m] == i) {
    logChar(markerChar);
    m++;
  }
  debug_write('\n');
}

static void logChar(char ch) {
  //  Log the character in hex e.g. '\r' becomes "[0x0d]"
  debug_print("[0x");
  debug_write((uint8_t) nibbleToHex[ch / 16]);
  debug_write((uint8_t) nibbleToHex[ch % 16]);
  debug_print("]");
}

#ifdef NOTUSED
static String toHex(char c) {
  //  Convert the char to a string of 2 hex digits.
  byte *b = (byte *) &c;
  String bytes;
  if (b[0] <= 0xF) bytes.concat('0');
  bytes.concat(String(b[0], 16));
  return bytes;
}
#endif  //  NOTUSED
