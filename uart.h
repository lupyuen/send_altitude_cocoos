#ifndef UART_H_
#define UART_H_

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define MODEM_BITS_PER_SECOND 9600  //  Connect to modem at this bps.

//  According to regulation, messages should be sent only every 10 minutes.
const unsigned long SEND_DELAY = (unsigned long) 10 * 60 * 1000;
const unsigned int MAX_BYTES_PER_MESSAGE = 12;  //  Only 12 bytes per message.
const unsigned int COMMAND_TIMEOUT = 1000;  //  Wait up to 1 second for response from SIGFOX module.

bool sendBuffer(
  const String &buffer, 
  unsigned long timeout,
  char markerChar,
  uint8_t expectedMarkerCount,
  String &response,
  uint8_t &actualMarkerCount);

void setup_uart(uint8_t rx, uint8_t tx, bool echo);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  UART_H_
