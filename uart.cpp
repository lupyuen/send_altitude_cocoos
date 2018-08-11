//  Functions to send and receive data from the UART serial port, e.g. for Wisol module.
#include <Arduino.h>
#include <SoftwareSerial.h>
#include "cocoos_cpp.h"
#include "uart.h"

void logBuffer(const __FlashStringHelper *prefix, const char *buffer, char markerChar,
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

bool sendBuffer(
  const String &buffer, 
  unsigned long timeout,
  char markerChar,
  uint8_t expectedMarkerCount,
  String &response,
  uint8_t &actualMarkerCount) {
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

  bool status = false;  //  Return status.
  int sendIndex = 0;  //  Index of next char to be sent.
  unsigned long sentTime = 0;  //  Timestamp at which we completed sending.
  uint8_t sendChar;
  actualMarkerCount = 0;
  response = "";
  serialPort->begin(MODEM_BITS_PER_SECOND);

  task_open();  //  Start of the task. Must be matched with task_close().

  //  Start the serial interface for the transceiver.
  log2(F(" - Wisol.sendBuffer: "), buffer);
  // log2(F("response / expectedMarkerCount / timeout: "), response + " / " + expectedMarkerCount + " / " + timeout);

  task_wait(delayAfterStart);

  //  Start listening for responses from the transceiver.
  //// serialPort->flush();
  serialPort->listen();

  //  Send the buffer char by char.
  for (;;) {
    //  If there is no data left to send, continue to next step.
    if (sendIndex >= buffer.length()) break;

    //  Send the next char.
    sendChar = (uint8_t) buffer.charAt(sendIndex);
    serialPort->write(sendChar);
    sendIndex++;
    // Serial.println(String("send: ") + String((char) sendChar) + " / " + String(toHex((char)sendChar))); ////
    task_wait(delayAfterSend);  //  Need to wait a while because SoftwareSerial has no FIFO and may overflow.
  }
  sentTime = millis();  //  Start the timer for detecting receive timeout.
  
  //  Read response.  Loop until timeout or we see the end of response marker.
  for (;;) {
    //  If receive step has timed out, quit.
    const unsigned long currentTime = millis();
    if (currentTime - sentTime > timeout) {
      logBuffer(F("<< (Timeout)"), "", markerChar, 0, 0);
      break;
    }
    //  No data is available in the serial port buffer to receive now.  We retry later.
    if (serialPort->available() <= 0) { continue; }

    //  Attempt to read the data.
    int receiveChar = serialPort->read();
    // Serial.println(String("receive: ") + String((char) receiveChar) + " / " + String(toHex((char)receiveChar))); ////

      //  No data is available now.  We retry.
    if (receiveChar == -1) { continue; }

    if (receiveChar == markerChar) {
      //  We see the "\r" marker. Remember the marker location so we can format the debug output.
      if (actualMarkerCount < markerPosMax) markerPos[actualMarkerCount] = response.length();
      actualMarkerCount++;  //  Count the number of end markers.

      //  We have encountered all the markers we need.  Stop receiving.
      if (actualMarkerCount >= expectedMarkerCount) break;
      continue;  //  Continue to receive next char.
    }

    //  Else append the received char to the response.
    response.concat(String((char) receiveChar));
    // Serial.println(String("response: ") + response); ////
    // log2(F("receiveChar "), receiveChar);
  }
  //  Finished the send and receive.  We close the serial port.
  //  In case of timeout, also close the serial port.
  serialPort->end();

  //  Log the actual bytes sent and received.
  //  log2(F(">> "), echoSend);
  //  if (echoReceive.length() > 0) { log2(F("<< "), echoReceive); }
  logBuffer(F(">> "), buffer.c_str(), markerChar, 0, 0);
  logBuffer(F("<< "), response.c_str(), markerChar, markerPos, actualMarkerCount);

  //  If we did not see the expected number of '\r', something is wrong.
  if (actualMarkerCount < expectedMarkerCount) {
    status = false;  //  Return failure.
    if (response.length() == 0) {
      log1(F(" - Wisol.sendBuffer: Error: No response"));  //  Response timeout.
    } else {
      log2(F(" - Wisol.sendBuffer: Error: Unknown response: "), response);
    }
  } else {
    status = true;  //  Return success.
    log2(F(" - Wisol.sendBuffer: response: "), response);
  }

  task_close();  //  End of the task.

  return status;
}

void setup_uart(uint8_t rx, uint8_t tx, bool echo) {
  //  Init the module with the specified transmit and receive pins.
  //  Default to no echo.
  //  void setup_uart(Country country0, bool useEmulator0, const String device0, bool echo,
  /*
  zone = 4;  //  RCZ4
  country = country0;
  useEmulator = useEmulator0;
  device = device0;
  */
  serialPort = new SoftwareSerial(rx, tx);
  if (echo) echoPort = &Serial;
  else echoPort = &nullPort;
  lastEchoPort = &Serial;
}

//  Convert nibble to hex digit.
static const char nibbleToHex[] = "0123456789abcdef";

void logBuffer(const __FlashStringHelper *prefix, const char *buffer, char markerChar,
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
