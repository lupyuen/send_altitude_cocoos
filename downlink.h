//  Your own function to process any received downlink messages.
#ifndef DOWNLINK_H_
#define DOWNLINK_H_
#include "platform.h"
#include "network.h"

BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.


bool process_downlink_msg(
  NetworkContext *context,  //  Task context.
  bool status,  //  True if downlink was received.
  const char *data);   //  Downlink data (8 bytes in hex e.g. "0102030405060708") or error name.

END_EXTERN_C //  End of extern C scope.

#endif  //  DOWNLINK_H_
