//  Stubs to allow Arduino programs to compile on STM32.
//  To be removed when porting is completed.
#ifndef FLASH_H_
#define FLASH_H_

//  No need for flash memory helpers on STM32.
#define __FlashStringHelper char
#define F(x) x

#endif  //  FLASH_H_
