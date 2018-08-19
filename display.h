#ifndef DISPLAY_H_
#define DISPLAY_H_

//  Uncomment to log sensor data to console via Display Task instead of sending to network.
//  #define SENSOR_DISPLAY

#include "platform.h"
#include <stdint.h>
#include <cocoos.h>
#define SERIAL_BAUD 9600  //  Serial Monitor will run at this bitrate.

#if defined(DISABLE_DEBUG_LOG)  //  If debug logging is disabled...

#define debug(p1, p2) {}
#define debug_begin(p1) {}
#define debug_print(p1) {}
#define debug_println(p1) {}
#define debug_flush() {}

#elif defined(ARDUINO)  //  Use Serial object to print.

#ifdef __cplusplus  //  Serial class for C++ only.
#define debug_begin(x) Serial.begin(x)
#define debug_print(x) Serial.print(x)
#define debug_println(x) Serial.println(x)
#define debug_flush() Serial.flush()
#endif  //  __cplusplus

#elif defined(STM32)  //  TODO: Define the debug log functions.
#include <stdlib.h>

BEGIN_EXTERN_C
void debug_begin(uint16_t bps);
void debug_print(const char *s);
void debug_println(const char *s);
void debug_flush(void);
END_EXTERN_C

#ifdef __cplusplus  //  Overload for C++
void debug_print(int i);
void debug_print(size_t l);
void debug_print(char ch);
void debug_print(float f);

void debug_println(int i);
void debug_println(size_t l);
void debug_println(char ch);
void debug_println(float f);
#endif  //  __cplusplus

#endif

#ifndef DISABLE_DEBUG_LOG  //  If debug logging is enabled...
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

//  Print a message to the Arduino serial console.  The function is overloaded to support
//  printing of strings in dynamic memory and strings in flash (e.g. F(...)).
void debug(
  const char *s1,  //  String in dynamic memory.
  const char *s2  //  Another string in dynamic memory.
  #ifdef __cplusplus
    = 0  //  Second parameter may be omitted.
  #endif
  );
END_EXTERN_C

#ifdef __cplusplus  //  Overloaded functions for C++ only, not C.
#ifdef ARDUINO  //  Flash memory for Arduino only.
void debug(
  const __FlashStringHelper *s1  //  String in flash memory e.g. F("the string")
);

void debug(
  const char *s1,  //  String in dynamic memory.
  const __FlashStringHelper *s2  //  String in flash memory e.g. F("the string")
);

void debug(
  const __FlashStringHelper *s1,  //  String in flash memory e.g. F("the string")
  const char *s2  //  String in dynamic memory.
);
#endif  //  ARDUINO
#endif  //  __cplusplus
#endif  //  !DISABLE_DEBUG_LOG

#ifdef SENSOR_DISPLAY  //  If display sensor data instead of sending to network...

#define SENSOR_DISPLAY_SIZE MAX_SENSOR_COUNT  //  Max number of sensors that can be displayed during a single refresh.
#define DISPLAY_MSG_POOL_SIZE 6  //  Allow only 6 display messages to be queued, which means fewer than 6 sensors allowed.
#define DISPLAY_MSG 34  //  TODO: Signal the display to update.

BEGIN_EXTERN_C

//  Interface for displaying sensor data.
struct Display {
  void (*refresh_func)(void);
  void (*update_data_func)(uint8_t id, const char *name, const float *data, uint8_t count);
};

Display *get_display(void);  //  Return the global instance of the display interface.
void init_display(void);  //  Initialise the display interface.
void display_task(void);  //  Display Task runs this function to display messages received.

END_EXTERN_C

#endif  //  SENSOR_DISPLAY
#endif  //  DISPLAY_H_
