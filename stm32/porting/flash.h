//  Stubs to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
#ifndef FLASH_H_
#define FLASH_H_

//  No need for flash memory helpers on STM32.
#define __FlashStringHelper char
#define F(x) x

// When compiling programs with this class, the following gcc parameters
// dramatically increase performance and memory (RAM) efficiency, typically
// with little or no increase in code size.
//     -felide-constructors
//     -std=c++0x

// typedef const char *PSTR;
// class __FlashStringHelper;
// #define F(string_literal) (reinterpret_cast<const __FlashStringHelper *>(PSTR(string_literal)))

#endif  //  FLASH_H_
