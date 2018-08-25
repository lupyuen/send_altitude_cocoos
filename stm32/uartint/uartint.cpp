//  UART Interface for STM32 UART port. Compatible with Arduino's SoftwareSerial.
#include <logger.h>
#include "uartint.h"

UARTInterface::UARTInterface(unsigned rx, unsigned tx) {
    //  TODO
}
void UARTInterface::begin(uint16_t bps) {
    //  TODO
}
void UARTInterface::listen() {
    //  TODO
}
int UARTInterface::available() { 
    //  TODO: Return number of bytes available for reading.
    return 0; 
}
int UARTInterface::read() { 
    //  TODO: Return the next byte, or -1 if none.
    debug_println("uart_read");
    return -1; 
}
void UARTInterface::write(uint8_t ch) {
    debug_println("uart_write");
    //  TODO
}
void UARTInterface::end() {
    //  TODO
}
