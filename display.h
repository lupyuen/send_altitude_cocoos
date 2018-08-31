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

//  Don't show debug log.
#define debug(p1, p2) {}
#define debug_begin(p1) {}
#define debug_write(p1) {}
#define debug_print(p1) {}
#define debug_println(p1) {}
#define debug_flush() {}

#elif defined(ARDUINO)  //  For Arduino...

//  Use Serial object to print. This conserves memory.
#ifdef __cplusplus  //  Serial class for C++ only.
#define debug_begin(x) Serial.begin(x)
#define debug_write(x) Serial.write(x)
#define debug_print(x) Serial.print(x)
#define debug_println(x) Serial.println(x)
#define debug_flush() Serial.flush()
#endif  //  __cplusplus

#else  //  For STM32 and other platforms...

//  Use logger functions defined in STM32 logger library.
#include <stdlib.h>  //  For size_t
BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.
void debug_begin(uint16_t bps);     //  Open the debug console at the specified bits per second.
void debug_write(uint8_t ch);       //  Write a character to the buffered debug log.
void debug_print(const char *s);    //  Write a string to the buffered debug log.
void debug_println(const char *s);  //  Write a string plus newline to the buffered debug log.
void debug_flush(void);             //  Flush the buffer of the debug log so that buffered data will appear.
END_EXTERN_C  //  End of extern C scope.

#ifdef __cplusplus  //  Overloaded debug functions for C++ only
//  Write an int / size_t / char / float to the buffered debug log.
void debug_print(int i);
void debug_print(size_t l);
void debug_print(char ch);
void debug_print(float f);  //  Note: Always prints with 2 decimal places.

//  Write an int / size_t / char / float plus newline to the buffered debug log.
void debug_println(int i);
void debug_println(size_t l);
void debug_println(char ch);
void debug_println(float f);  //  Note: Always prints with 2 decimal places.
#endif  //  __cplusplus

#ifdef STM32  //  Include the STM32 logger declarations to confirm they confirm to above declarations.
#include "logger.h"
#endif  //  STM32
#endif  //  DISABLE_DEBUG_LOG, ARDUINO

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
END_EXTERN_C  //  End of extern C scope.

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
