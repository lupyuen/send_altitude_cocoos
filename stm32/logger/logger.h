//  Log messages to the debug console.  Note: Logging will be buffered in memory.  Messages will not 
//  be displayed until debug_flush() is called.
#ifndef LOGGER_H_
#define LOGGER_H_
#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif
void enable_log(void);   //  Enable the debug log.
void disable_log(void);  //  Disable the debug log.
#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#if defined(DISABLE_DEBUG_LOG)  //  If debug logging is disabled...

//  Don't show debug log.
#define debug(p1, p2) {}
#define debug_begin(p1) {}
#define debug_write(p1) {}
#define debug_print(p1) {}
#define debug_println(p1) {}
#define debug_append(p1, p2) {}
#define debug_flush() {}

#else  //  If debug logging is enabled...

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif
void debug_begin(uint16_t bps);     //  Open the debug console at the specified bits per second.
void debug_write(uint8_t ch);       //  Write a character to the buffered debug log.
void debug_print(const char *s);    //  Write a string to the buffered debug log.
void debug_println(const char *s);  //  Write a string plus newline to the buffered debug log.
void debug_printhex(uint8_t ch);    //  Write a char in hexadecimal to the buffered debug log.
void debug_append(const char *buffer, unsigned int length);  //  //  Append "length" number of bytes from "buffer" to the debug buffer.
void debug_flush(void);             //  Flush the buffer of the debug log so that buffered data will appear.
#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#ifdef __cplusplus  //  Overloaded debug functions for C++ only
//  Write an int / size_t / char / float to the buffered debug log.  Will not be displayed until debug_flush() is called.
void debug_print(int i);
void debug_print(size_t l);
void debug_print(char ch);
void debug_print(float f);  //  Note: Always prints with 2 decimal places.

//  Write an int / size_t / char / float plus newline to the buffered debug log.  Will not be displayed until debug_flush() is called.
void debug_println(int i);
void debug_println(size_t l);
void debug_println(char ch);
void debug_println(float f);  //  Note: Always prints with 2 decimal places.  Will not be displayed until debug_flush() is called.

#endif  //  __cplusplus
#endif  //  DISABLE_DEBUG_LOG
#endif  //  LOGGER_H_
