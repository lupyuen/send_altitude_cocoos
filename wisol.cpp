//  Functions to send and receive messages to/from Sigfox network via 
//  Wisol modules WSSFM10R1AT, WSSFM10R2AT, WSSFM10R3AT and WSSFM10R4AT
//  for Sigfox zones RCZ1, RCZ2, RCZ3 and RCZ4 respectively.
#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "sensor.h"
#include "aggregate.h"
#include "downlink.h"
#include "wisol.h"

#include "radio.h"








/* Downlink Server Support: https://backend.sigfox.com/apidocs/callback
When a message needs to be acknowledged, the callback selected for the downlink data must 
send data in the response. It must contain the 8 bytes data that will be sent to the device 
asking for acknowledgment. The data is json formatted, and must be structured as the following :
  {"YOUR_DEVICE_ID" : { "downlinkData" : "deadbeefcafebabe"}}    
With YOUR_DEVICE_ID being replaced by the corresponding device id, in hexadecimal format, up to 8 digits. 
The downlink data must be 8 bytes in hexadecimal format.  For example:
  {"002C2EA1" : { "downlinkData" : "0102030405060708"}} */




