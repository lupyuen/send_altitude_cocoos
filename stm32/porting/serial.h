//  The code here reproduces the Arduino class Serial.
//  To be removed when we no longer use Serial class.
#ifndef SERIAL_H_
#define SERIAL_H_
#ifdef __cplusplus  //  Serial class for C++ only.
// #include <stdio.h>   //  For printf()
// #include <wstring.h> //  For Arduino String class

class Print {  //  Reproduce the Arduino base class for Serial class.
public:
  Print() {}
  Print(unsigned rx, unsigned tx) {}
  void begin(int i) {}
  void print(const char *s); // { printf(s); }
  void print(const String &s); // { printf(s.c_str()); }
  void print(int i); // { printf("%d", i); }
  void print(float f); // { printf("%f", f); }
  void print(size_t z); // { printf("%zu", z); }
  void println(const char *s); // { puts(s); }
  void println(const String &s); // { puts(s.c_str()); }
  void println(int i); // { printf("%d\n", i); }
  void println(float f); // { printf("%f\n", f); }
  void println(size_t z); // { printf("%zu\n", z); }
  void flush() {}
  void listen() {}
  void write(uint8_t ch); // { putchar(ch); }
  int read() { return -1; }
  bool available() { return false; }
  void end() {}
};
extern Print Serial;

inline bool operator!(Print &p) {
  //  Allow wait for the Serial interface to be ready:
  //  while (!Serial) {}
  return false;
}

#endif  //  __cplusplus
#endif  //  SERIAL_H_
