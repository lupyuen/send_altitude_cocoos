//  Log messages to the debug console.
#ifndef LOGGER_H_
#define LOGGER_H_
#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif
void enable_log(void);
void disable_log(void);
void debug_begin(uint16_t bps);
void debug_write(uint8_t ch);
void debug_print(const char *s);
void debug_println(const char *s);
void debug_flush(void);
#ifdef __cplusplus
}  //  End of extern C scope.
#endif

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
#endif  //  LOGGER_H_
