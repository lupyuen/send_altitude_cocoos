/*
 * Pin.cpp
 *
 */

#include <pin.h>
#include <config.h>
#include <inc/stm32f4xx.h>

static GPIO_TypeDef * const gpios[] = {GPIOA, GPIOB, GPIOC, GPIOD, GPIOE, GPIOF};
static uint16_t const pins[] = { GPIO_Pin_0, GPIO_Pin_1, GPIO_Pin_2, GPIO_Pin_3,
                                 GPIO_Pin_4, GPIO_Pin_5, GPIO_Pin_6, GPIO_Pin_7,
                                 GPIO_Pin_8, GPIO_Pin_9, GPIO_Pin_10, GPIO_Pin_11,
                                 GPIO_Pin_12, GPIO_Pin_13, GPIO_Pin_14, GPIO_Pin_15 };






pin::pin() {
    // TODO Auto-generated constructor stub

}


void pin::init() {
    for ( int i = 0; i != n_configured_pins; ++i ) {
        cfg_t *pcfg = pinConfigs[ i ];

        if ( pcfg->pcfg.GPIO_Mode == GPIO_Mode_AF ) {
            GPIO_PinAFConfig(pcfg->port, pcfg->af.GPIO_PinSource, pcfg->af.GPIO_AF);
        }

        GPIO_Init(pcfg->port, &pcfg->pcfg);

    }
}


void pin::setPin( pin::ports_t port, pin::pins_t pin ) {
    GPIO_SetBits( gpios[ port ], pins[ pin ]);
}


void pin::setPin( pin::pinfunc_t pin ) {
    GPIO_SetBits( pinConfigs[ pin ]->port, pinConfigs[ pin ]->pcfg.GPIO_Pin );
}


void pin::resetPin( pin::ports_t port, pin::pins_t pin ) {
    GPIO_ResetBits( gpios[ port ], pins[ pin ]);
}


void pin::resetPin( pin::pinfunc_t pin ) {
    GPIO_ResetBits( pinConfigs[ pin ]->port, pinConfigs[ pin ]->pcfg.GPIO_Pin );
}


uint8_t pin::readPin( pin::ports_t port, pin::pins_t pin ) {
    return GPIO_ReadInputDataBit(gpios[ port ], pins[ pin ]);
}


uint8_t pin::readPin( pin::pinfunc_t pin ) {
    return GPIO_ReadInputDataBit( pinConfigs[ pin ]->port, pinConfigs[ pin ]->pcfg.GPIO_Pin );
}


void pin::togglePin( pin::ports_t port, pin::pins_t pin ) {
    if ( readPin( port, pin ) == Bit_SET ) {
        resetPin( port, pin );
    }
    else {
        setPin( port, pin );
    }
}


void pin::togglePin( pin::pinfunc_t pin ) {
    if ( readPin( pin ) == Bit_SET ) {
        resetPin( pin );
    }
    else {
        setPin( pin );
    }
}


