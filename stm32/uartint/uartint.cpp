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
    //  TODO
    return 1; 
}
int UARTInterface::read() { 
    //  TODO
    debug_println("uart_read");
    return 30; 
}
void UARTInterface::write(uint8_t ch) {
    debug_println("uart_write");
    //  TODO
}
void UARTInterface::end() {
    //  TODO
}
