#ifndef STM32F4_USART_H_
#define STM32F4_USART_H_

#include <stdint.h>
#include <cstddef>



class usart {
public:
    typedef usart *ptr;
    typedef uint16_t ID;

    usart( ID id );
    ~usart();

    // Get a pointer to the instance
    static ptr instance( ID id );

    static const uint8_t N_MAX_USARTS = 6;

    // Callback interface for rx isr
    // An uart reader must implement this interface to
    // receive uart data
    class IReaderCb {
    public:
        virtual void usart_update(uint8_t data) = 0;
    };

    // Register a uart listener
    void registerReader(IReaderCb* rd);

    bool write(uint8_t *buf, uint8_t len);

    void isr();
    struct cfg_t;

private:

    void init();

    // static list of uart configurations
    static const cfg_t *settings[];
    static const uint16_t n_configured_usarts;

    uint8_t irqnum( ID id );

    uint8_t *txdata;
    IReaderCb *reader;
    ID m_id;
    uint8_t n_write;
    uint8_t *next;
    bool busy;
};




#endif /* STM32F4_USART_H_ */
