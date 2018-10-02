//  Functions to send and receive data from the UART serial port, e.g. for Wisol module.
#include "platform.h"
#ifdef DISABLE_UART_LOG
#define DISABLE_DEBUG_LOG  //  Disable debug logging for the UART Task.
#endif  //  DISABLE_UART_LOG
#include <string.h>
#include <cocoos.h>
#include "display.h"
#include "sensor.h"
#include "uart.h"

#ifdef STM32                    //  If we are compiling for STM32 Blue Pill...
#include <boost_lockfree.hpp>   //  Force boost_lockfree library to be included.  Mandatory.
#include <uartint.h>            //  For UARTInterface
#include <ports/bluepill/micropython.h> //  TODO: MicroPython
#endif  //  STM32

#ifdef ARDUINO               //  If we are compiling for Arduino...
#include <SoftwareSerial.h>  //  Use the software implementation of the serial port.
#endif  //  ARDUINO

#ifdef TRANSMIT_SENSOR_DATA  //  The code below is only used if we are transmitting data to the network.

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
static const uint16_t delayAfterStart =  200;  //  Delay after UART port init.
static const uint16_t delayAfterSend  =   10;  //  Delay after sending data.
static const uint16_t delayReceive    = 1000;  //  Delay while receiving data.

//  Remember where in response the '\r' markers were seen.
const uint8_t markerPosMax = 5;
static uint8_t markerPos[markerPosMax];

#ifdef ARDUINO
SoftwareSerial serialPort(UART_RX_PORT, UART_TX_PORT);  //  Serial port for send/receive.
#else
UARTInterface serialPort(UART_RX_PORT, UART_TX_PORT);  //  UART port for send/receive.
#endif  //  ARDUINO

//  We define ctx() as a shortcut for fetching the UARTContext for the UART Task.
//  We use a macro instead of declaring a variable because the context needs to be refetched
//  after calling cocoOS functions that may switch the task context, e.g. sem_wait().
#define ctx() ((UARTContext *) task_get_data())

void uart_task(void) {
  //  This task loops and waits for an incoming message containing UART data to be sent.
  //  sendData contains a string of ASCII chars to be sent to the UART port.
  //  We send the sendData to the port.  expectedMarkerCount is the number of 
  //  end-of-command markers '\r' we expect to see.  actualMarkerCount contains the 
  //  actual number of end-of-command seen. We trigger to the caller the events 
  //  successEvent or failureEvent depending on success/failure sending the data.  
  //  Response is recorded in the "response" variable of the context, for the caller to retrieve.
  //  UART Task is designed to be reusable for various types of UART processing.
  static UARTMsg msg;  //  The received message.
  uint8_t sendChar;    //  Character to be sent.

  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) {    //  Run the UART sending code forever. So the task never ends.
    msg_receive(os_get_running_tid(), &msg);  //  Wait until we receive a UART message.
    ctx()->msg = &msg;  //  Remember the message until it's transmitted to the UART port.
    logBuffer(F(">> "), ctx()->msg->sendData, ctx()->msg->markerChar, 0, 0); debug_flush();
    //  log2(F(" - uart.sendData: "), ctx()->msg->sendData); log2(F("expectedMarkerCount / timeout: "), String(ctx()->msg->expectedMarkerCount) + String(F(" / ")) + String(ctx()->msg->timeout));

    //  Initialise the context for the task. These variables will change while sending.
    ctx()->status = true;    //  Assume the return status will be successful.
    ctx()->sendIndex = 0;    //  Index of next char to be sent.
    ctx()->sentTime = 0;     //  Timestamp at which we completed sending.
    ctx()->response[0] = 0;  //  Empty the response buffer.
    ctx()->actualMarkerCount = 0;  //  How many markers we actually received.

    //  Initialise the UART port.
    serialPort.begin(UART_BITS_PER_SECOND);  //  Start the UART interface.
    task_wait(delayAfterStart);  //  Wait for the UART port to be ready.
    serialPort.listen();        //  Start listening for responses from the UART port.
    
    ///////////////////////////////////////////////////////////////////////////
    //  UART Transmit Loop
    for (;;) {  //  Transmit sendData char by char to the UART port.
      //  If there is no data left to send, continue to the receive step.
      if (ctx()->sendIndex >= strlen(ctx()->msg->sendData)
        || ctx()->sendIndex >= MAX_UART_SEND_MSG_SIZE) { break; }
      //  Send the next char.
      sendChar = (uint8_t) ctx()->msg->sendData[ctx()->sendIndex];
      serialPort.write(sendChar);
      ctx()->sendIndex++;  //  debug_println(String(F("send: ")) + String((char) sendChar) + String(F(" / ")) + String(toHex((char)sendChar))); ////
      task_wait(delayAfterSend);  //  Need to wait a while because SoftwareSerial has no FIFO and may overflow.
    }
    ctx()->sentTime = millis();   //  Start the timer for detecting receive timeout.

    ///////////////////////////////////////////////////////////////////////////
    //  UART Receive Loop
    for (;;) {  //  Read the response from the UART port.  Loop until timeout or we see the end-of-command marker "\r".
      unsigned long currentTime, elapsedTime, remainingTime;
      currentTime = millis();
      elapsedTime = currentTime - ctx()->sentTime;  //  Compute the elapsed time since the start of the receive.
      if (serialPort.available() <= 0) { 
        //  No data is available in the serial port sendData to receive now.  We retry later.
        if (elapsedTime > ctx()->msg->timeout) {
          //  If we have waited too long to receive, quit.
          debug_print(F("<< uart timeout: elapsed ")); debug_print((size_t) elapsedTime); debug_print(F(" timeout ")); debug_print((size_t) ctx()->msg->timeout);
          logBuffer(F(" "), "", ctx()->msg->markerChar, 0, 0);
          break;
        }
        //  Wait a while before checking receive.
        remainingTime = ctx()->msg->timeout - elapsedTime;
        if (remainingTime > (2 * delayReceive)) {  //  Wait only if there is twice the time remaining.
          task_wait(delayReceive); 
        }
        continue;  //  Check again to receive next char.
      }
      //  Note: If there is data to be read even though the timeout has been reached, we still continue to read.
      //  Attempt to read the data.
      int receiveChar = serialPort.read();  //  debug_println(String("receive: ") + String((char) receiveChar) + " / " + String(toHex((char)receiveChar))); ////

      //  No data is available now.  We retry.
      if (receiveChar == -1) { continue; }  //  Actually should not come here, since the above conditions covered the no-data case.

      if (receiveChar == ctx()->msg->markerChar) {
        //  We see the end-of-command marker "\r".
        rememberMarker(ctx());       //  Remember the marker location so we can format the debug output.
        ctx()->actualMarkerCount++;  //  Count the number of end-of-command markers seen.
        //  If we have encountered all the markers we need, stop receiving.
        if (ctx()->actualMarkerCount >= ctx()->msg->expectedMarkerCount) { break; }
        continue;  //  Else continue to receive next char.
      }
      //  If not end-of-command marker "\r", append the received char to the response.
      int len = strlen(ctx()->response);
      if (len >= MAX_UART_RESPONSE_MSG_SIZE) {
        debug_print(F("***** Error: UART response overflow - ")); debug_println(len);
      } else {
        ctx()->response[len] = (char) receiveChar;
        ctx()->response[len + 1] = 0;
      }  //  debug_println(String(F("response: ")) + ctx()->response); log2(F("receiveChar "), receiveChar);
    }    //  Loop until receive is complete or timeout.

    ///////////////////////////////////////////////////////////////////////////
    //  UART Send/Receive Finished
    serialPort.end();  //  We are finished with the UART send/receive.  Close the UART port.

    //  If we did not see the expected number of '\r' markers, record the error.
    if (ctx()->actualMarkerCount < ctx()->msg->expectedMarkerCount) { ctx()->status = false; }
    logSendReceive(ctx());  //  Log the status and actual bytes sent and received.

    if (ctx()->msg->responseMsg != NULL) {
      //  If caller has requested for response message, then send it instead of event.
      msg_post(ctx()->msg->responseTaskID, *(ctx()->msg->responseMsg));
    } else if (ctx()->status == true) {
      //  If no error, trigger the success event to caller.
      //  The caller can read the response from the context.response.
      event_signal(ctx()->msg->successEvent);      
    } else {
      //  If we hit an error, trigger the failure event to the caller.
      event_signal(ctx()->msg->failureEvent);  //  Trigger the failure event.
    }
    #ifdef NOTUSED  //  Test whether the timer is accurate while multitasking.
    ctx()->testTimer = millis();
    task_wait(10); TEST_TIMER(10);  //  10 milliseconds. First time may not be accurate.
    task_wait(10); TEST_TIMER(10);  //  10 milliseconds
    task_wait(100); TEST_TIMER(100);  //  100 milliseconds
    task_wait(200); TEST_TIMER(200);  //  200 milliseconds
    #endif       //  NOTUSED
  }              //  Loop back and wait for next queued message.
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

static void logBuffer(
  const __FlashStringHelper *prefix, 
  const char *data, 
  char markerChar,
  uint8_t *markerPos, 
  uint8_t markerCount) {
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

#endif  //  TRANSMIT_SENSOR_DATA
