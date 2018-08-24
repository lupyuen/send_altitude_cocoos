#ifndef _UART_SERIAL_H_
#define _UART_SERIAL_H_

#include <stdint.h>



class UartSerial {
public:
    typedef UartSerial *ptr;
    typedef uint16_t ID;

    UartSerial( ID id );
    ~UartSerial();

    // Get a pointer to the instance
    static ptr instance( ID id );

    static const uint8_t N_MAX_USARTS = 6;

    // Callback interface for rx isr
    // An uart reader must implement this interface to
    // receive uart data
    class IReaderCb {
    public:
        virtual void update(uint8_t data) = 0;
    };

    // Register a uart listener
    void registerReader(IReaderCb* rd);

    bool write(const uint8_t *buf, uint8_t len);

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




#endif /* _SERIAL_DEVICE_H_ */
