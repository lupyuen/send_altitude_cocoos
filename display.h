//  Display the debug log on the console.  If SENSOR_DISPLAY is enabled,
//  there will be a Display Task that will receive sensor data messages
//  and displays them on the console.
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
#define debug_write(p1) {}
#define debug_print(p1) {}
#define debug_println(p1) {}
#define debug_flush() {}

#elif defined(ARDUINO)  //  Use Serial object to print.

#ifdef __cplusplus  //  Serial class for C++ only.
#define debug_begin(x) Serial.begin(x)
#define debug_write(x) Serial.write(x)
#define debug_print(x) Serial.print(x)
#define debug_println(x) Serial.println(x)
#define debug_flush() Serial.flush()
#endif  //  __cplusplus

#elif defined(STM32)  //  Use logger functions defined in logger library.
#include "logger.h"
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

#if defined(__cplusplus) && defined(ARDUINO)  //  Overloaded Flash functions for C++ only, not C.
void debug(const __FlashStringHelper *s1); //  String in flash memory e.g. F("the string")
void debug(const char *s1, const __FlashStringHelper *s2); //  Dynamic memory + flash memory
void debug(const __FlashStringHelper *s1, const char *s2); //  Flash memory + dynamic memory
#endif  //  __cplusplus && ARDUINO
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
