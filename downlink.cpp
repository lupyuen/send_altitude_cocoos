//  Your own function to process any received downlink messages.
#include "platform.h"

#include "downlink.h"

bool process_downlink_msg(
  NetworkContext *context,  //  Task context.
  bool status,  //  True if downlink was received.
  const char *data) {  //  Downlink data (up to 8 bytes in hex e.g. "0102030405060708") or error name.

  //  TODO: Add your code here to process the downlink message.

  return true;  //  Means no error.
}
