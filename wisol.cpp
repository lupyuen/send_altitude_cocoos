//  Functions to send and receive messages to/from Sigfox network via Wisol module.
#include <Arduino.h>
#include "cocoos_cpp.h"
#include "uart.h"
#include "wisol.h"

#define END_OF_RESPONSE '\r'  //  Character '\r' marks the end of response.

static int zone;  //  1 to 4 representing SIGFOX frequencies RCZ 1 to 4.
static Country country;   //  Country to be set for SIGFOX transmission frequencies.
static bool useEmulator;  //  Set to true if using UnaBiz Emulator.
static String device;  //  Name of device if using UnaBiz Emulator.

void setup_wisol(Country country0, bool useEmulator0, const String device0) {
  //  Init the module.
  zone = 4;  //  RCZ4
  country = country0;
  useEmulator = useEmulator0;
  device = device0;
}
