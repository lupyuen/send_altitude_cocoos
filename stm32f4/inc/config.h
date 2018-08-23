#ifndef CONFIG_H_
#define CONFIG_H_

#include <inc/stm32f4xx.h>
#include <stdint.h>
#include <pin.h>

#include "uartSerial.h"



/*=====================  PORTS/PINS CONFIGURATION  =====================*/

struct af_t {
    uint16_t GPIO_PinSource;
    uint8_t GPIO_AF;
};

struct pin::cfg_t {
    GPIO_TypeDef* port;
    GPIO_InitTypeDef pcfg;
    af_t af;                ///< only used when pin configured as alternate function
};




/*=====================  USART CONFIGURATION  =====================*/

#define DEBUG_USART_ID  0

struct UartSerial::cfg_t {
    USART_TypeDef* usart;
    USART_InitTypeDef init;
    NVIC_InitTypeDef nvic;
    uint8_t *out;
    uint16_t bufsz;
};






#endif /* CONFIG_H_ */
