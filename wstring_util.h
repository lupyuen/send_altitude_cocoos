#ifndef WSTRING_UTIL_H_
#define WSTRING_UTIL_H_
#ifndef ARDUINO  //  Reproduce Arduino conversion functions

#include "platform.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>

char *ltoa(long num, char *str, int radix) {
  char sign = 0;
  char temp[33];  //an int can only be 32 bits long
  //at radix 2 (binary) the string
  //is at most 16 + 1 null long.
  int temp_loc = 0;
  int digit;
  int str_loc = 0;

  //save sign for radix 10 conversion
  if (radix == 10 && num < 0) {
    sign = 1;
    num = -num;
  }

  //construct a backward string of the number.
  do {
    digit = (unsigned long)num % radix;
    if (digit < 10)
      temp[temp_loc++] = digit + '0';
    else
      temp[temp_loc++] = digit - 10 + 'A';
    num = ((unsigned long)num) / radix;
  } while ((unsigned long)num > 0);

  //now add the sign for radix 10
  if (radix == 10 && sign) {
    temp[temp_loc] = '-';
  } else {
    temp_loc--;
  }


  //now reverse the string.
  while ( temp_loc >=0 ) {// while there are still chars
    str[str_loc++] = temp[temp_loc--];
  }
  str[str_loc] = 0; // add null termination.

  return str;
}

char *utoa(unsigned num, char *str, int radix) {
  char temp[17];  //an int can only be 16 bits long
  //at radix 2 (binary) the string
  //is at most 16 + 1 null long.
  int temp_loc = 0;
  int digit;
  int str_loc = 0;

  //construct a backward string of the number.
  do {
    digit = (unsigned int)num % radix;
    if (digit < 10)
      temp[temp_loc++] = digit + '0';
    else
      temp[temp_loc++] = digit - 10 + 'A';
    num = ((unsigned int)num) / radix;
  } while ((unsigned int)num > 0);

  temp_loc--;


  //now reverse the string.
  while ( temp_loc >=0 ) {// while there are still chars
    str[str_loc++] = temp[temp_loc--];
  }
  str[str_loc] = 0; // add null termination.

  return str;
}

char *itoa(int num, char *str, int radix) {
  char sign = 0;
  char temp[17];  //an int can only be 16 bits long
  //at radix 2 (binary) the string
  //is at most 16 + 1 null long.
  int temp_loc = 0;
  int digit;
  int str_loc = 0;

  //save sign for radix 10 conversion
  if (radix == 10 && num < 0) {
    sign = 1;
    num = -num;
  }

  //construct a backward string of the number.
  do {
    digit = (unsigned int)num % radix;
    if (digit < 10)
      temp[temp_loc++] = digit + '0';
    else
      temp[temp_loc++] = digit - 10 + 'A';
    num = (((unsigned int)num) / radix);
  } while ((unsigned int)num > 0);

  //now add the sign for radix 10
  if (radix == 10 && sign) {
    temp[temp_loc] = '-';
  } else {
    temp_loc--;
  }


  //now reverse the string.
  while ( temp_loc >=0 ) {// while there are still chars
    str[str_loc++] = temp[temp_loc--];
  }
  str[str_loc] = 0; // add null termination.

  return str;
}

char *ultoa(unsigned long num, char *str, int radix) {
  char temp[33];  //an int can only be 16 bits long
  //at radix 2 (binary) the string
  //is at most 16 + 1 null long.
  int temp_loc = 0;
  int digit;
  int str_loc = 0;

  //construct a backward string of the number.
  do {
    digit = (unsigned long)num % radix;
    if (digit < 10)
      temp[temp_loc++] = digit + '0';
    else
      temp[temp_loc++] = digit - 10 + 'A';
    num = ((unsigned long)num) / radix;
  } while ((unsigned long)num > 0);

  temp_loc--;


  //now reverse the string.
  while ( temp_loc >=0 ) {// while there are still chars
    str[str_loc++] = temp[temp_loc--];
  }
  str[str_loc] = 0; // add null termination.

  return str;
}

char *dtostrf(double value, unsigned char d1, unsigned char d2, char *buf) {
  return (char *) "888";
}

#define strcpy_P strcpy
#define strlen_P strlen

class Print {
public:
  Print() {}
  Print(unsigned rx, unsigned tx) {}
  void begin(int i) {}
  void print(const char *s) { printf(s); }
  void print(const String &s) { printf(s.c_str()); }
  void print(int i) { printf("%d", i); }
  void print(float f) { printf("%f", f); }
  void println(const char *s) { puts(s); }
  void println(const String &s) { puts(s.c_str()); }
  void println(int i) { printf("%d\n", i); }
  void println(float f) { printf("%f\n", f); }
  void flush() {}
  void listen() {}
  void write(uint8_t ch) { putchar(ch); }
  int read() { return -1; }
  bool available() { return false; }
  void end() {}
};
Print Serial;

class SoftwareSerial: public Print {
public:
  SoftwareSerial(unsigned rx, unsigned tx): Print(rx, tx) {}
};

unsigned long millis() {
  return (unsigned long) clock();
}

void delay(long i) {  //  Milliseconds.
  const unsigned long start = millis();
  for (;;) {
    if (millis() - start > i) return;
  }
}

typedef uint8_t byte;

#endif  //  !ARDUINO
#endif  //  WSTRING_UTIL_H_
