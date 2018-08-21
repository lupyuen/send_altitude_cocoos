//  Reproduce Arduino conversion functions. Used by wstring.cpp.
#ifndef UTIL_H_
#define UTIL_H_
#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

#define strcpy_P strcpy
#define strlen_P strlen

typedef const char *PGM_P;

char *ltoa(long num, char *str, int radix);
char *utoa(unsigned num, char *str, int radix);
char *itoa(int num, char *str, int radix);
char *ultoa(unsigned long num, char *str, int radix);
char *dtostrf(double value, unsigned char d1, unsigned char d2, char *buf);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  UTIL_H_
