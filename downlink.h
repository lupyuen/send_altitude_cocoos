//  Your own function to process any received downlink messages.
#ifndef DOWNLINK_H_
#define DOWNLINK_H_

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

struct WisolContext;  //  Forward declaration

bool processDownlinkMsg(
  WisolContext *context,  //  Task context.
  bool status,  //  True if downlink was received.
  const char *data);   //  Downlink data (8 bytes in hex e.g. "0102030405060708") or error name.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  DOWNLINK_H_
