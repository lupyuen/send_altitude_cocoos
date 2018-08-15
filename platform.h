//  Load the platform-dependent system include files, e.g. Arduino.h
//  Also define the Serial buffer size.  Use platform.h instead of Arduino.h
//  for portability.
#ifndef PLATFORM_H_
#define PLATFORM_H_

//  Uncomment to use real sensor data instead of simulated data.
#define SENSOR_DATA

//  Uncomment to use simulated sensor data instead of real data.
//  #define SIMULATED_DATA

#define MAX_UART_SEND_MSG_SIZE 35  //  Max message length, e.g. 33 chars for AT$SF=0102030405060708090a0b0c,1\r
#define MAX_UART_RESPONSE_MSG_SIZE 36  //  Max response length, e.g. 36 chars for ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT\r

#ifdef ARDUINO  //  For Arduino only

//  Reduce the Serial buffer size from 64 to 16 to reduce RAM usage.
#define SERIAL_TX_BUFFER_SIZE 16
#define SERIAL_RX_BUFFER_SIZE 16

//  Reduce SoftwareSerial buffer size from 64 to max response length.
#define _SS_MAX_RX_BUFF MAX_UART_RESPONSE_MSG_SIZE

//  Define the Serial class.
#include <Arduino.h>

#endif  //  ARDUINO

//  TODO: Define other platforms e.g. STM32

#endif  //  PLATFORM_H_
