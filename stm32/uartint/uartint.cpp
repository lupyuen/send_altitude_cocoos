//  UART Interface for STM32 UART port. Compatible with Arduino's SoftwareSerial.
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
bool UARTInterface::available() { 
    //  TODO
    return false; 
}
int UARTInterface::read() { 
    //  TODO
    return -1; 
}
void UARTInterface::write(uint8_t ch) {
    //  TODO
}
void UARTInterface::end() {
    //  TODO
}
