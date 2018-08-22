#ifndef PIN_H_
#define PIN_H_

#include <stdint.h>



class pin {

public:

    enum ports_t {  PORT_A, PORT_B, PORT_C, PORT_D,
                    PORT_E, PORT_F, _NO_PORTS
                  };

    enum pins_t { PIN0, PIN1, PIN2, PIN3,
                  PIN4, PIN5, PIN6, PIN7,
                  PIN8, PIN9, PIN10, PIN11,
                  PIN12, PIN13, PIN14, PIN15, _NO_PINS

                };

    typedef uint16_t pinfunc_t;

    static void init();

    static void setPin( pin::ports_t port, pin::pins_t pin );
    static void setPin( pin::pinfunc_t pin );

    static void resetPin( pin::ports_t port, pin::pins_t pin );
    static void resetPin( pin::pinfunc_t pin );

    static uint8_t readPin( pin::ports_t port, pin::pins_t pin );
    static uint8_t readPin( pin::pinfunc_t pin );

    static void togglePin( pin::ports_t port, pin::pins_t pin );
    static void togglePin( pin::pinfunc_t pin );

    struct cfg_t;

    static const uint16_t n_configured_pins;
private:
    pin();
    static cfg_t *pinConfigs[];
};


#endif /* PIN_H_ */
