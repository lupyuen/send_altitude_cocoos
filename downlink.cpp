//  Your own function to process any received downlink messages.
#include "platform.h"
#include "display.h"
#include "downlink.h"

bool processDownlinkMsg(
  WisolContext *context,  //  Task context.
  bool status,  //  True if downlink was received.
  const char *data) {  //  Downlink data (up to 8 bytes in hex e.g. "0102030405060708") or error name.
  if (status) { debug(F(" - processDownlinkMsg: "), data); }
  else { debug(F(" - processDownlinkMsg (Failed): "), data); }

  //  TODO: Add your code here to process the downlink message.

  return true;  //  Means no error.
}
