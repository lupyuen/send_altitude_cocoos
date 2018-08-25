//  Platform-specific functions for Arduino.  Where's the rest of the code? Look in main.cpp.
//  Note: setup() and loop() will not be called since main() is defined (in main.cpp).
//  If you wish, you may use this file as a staging area while porting your Arduino code 
//  to .h and .cpp files. That's how bme280.h and bme280.cpp were created.
#include "platform.h"
#include "display.h"  //  For debug()
#include "sensor.h"  //  Defines SENSOR_DATA
#ifdef SENSOR_DATA  //  If we are using data from sensors, not hardcoded data...
#include <SPI.h>  //  Needed by BME280 library.
#endif  //  SENSOR_DATA

#ifdef ARDUINO  //  Arduino only

//  This is the tick function we will call every millisecond.  Usually points to os_tick() in cocoOS.
static void (*tickFunc)(void) = NULL;
static volatile uint32_t tickCount = 0;  //  Number of millisecond ticks elapsed.

void platform_setup(void) {
  //  Initialise the Arduino timers, since we are using main() instead of setup()+loop().
  init();  //  From Arduino system library
  debug(F("----platform_setup"));
  led_setup();  //  Get ready to blink the LED.
}

void platform_start_timer(void (*tickFunc0)(void)) {
  //  Start the AVR Timer 1 to generate interrupt ticks every millisecond
  //  for cocoOS to perform task switching.  AVR Timer 0 is reserved for 
  //  Arduino timekeeping. From https://arduinodiy.wordpress.com/2012/02/28/timer-interrupts/
  tickFunc = tickFunc0;
  cli();          //  Disable global interrupts
  TCCR1A = 0;     //  Set entire TCCR1A register to 0
  TCCR1B = 0;     //  Same for TCCR1B 
  OCR1A = 16000;    //  Set compare match register to desired timer count  
  TCCR1B |= (1 << WGM12);  //  Turn on CTC mode (Clear Timer on Compare Match)
  TCCR1B |= (1 << CS10);   //  Set timer prescaler as 1 (fastest)
  // TCCR1B |= (1 << CS11);   //  Set timer prescaler as 8 (fast)
  TIMSK1 |= (1 << OCIE1A);  //  Enable timer compare interrupt
  sei();          //  Enable global interrupts
}

ISR(TIMER1_COMPA_vect) {
  //  Handle the AVR Timer 1 interrupt. Trigger an os_tick() for cocoOS to perform task switching.
  ////  debug(F("os_tick")); ////
  tickCount++;
  if (tickFunc != NULL) tickFunc();
}

bool ledIsOn = false;  //  True if LED is on.

void led_setup(void) {
  //  Initialise the Arduino Uno's onboard LED at D13 for output.
  pinMode(LED_BUILTIN, OUTPUT);
}

void led_on(void) {
  // Turn the LED on (HIGH is the voltage level).
  digitalWrite(LED_BUILTIN, HIGH);
  ledIsOn = true;
}

void led_off(void) {
  // Turn the LED off (LOW is the voltage level).
  digitalWrite(LED_BUILTIN, LOW);
  ledIsOn = false;
}

void led_toggle(void) {
  //  Toggle the LED on and off.
  if (ledIsOn) { led_off(); }
  else led_on();
}

//  TODO: Enable and disable display of debug messages.
void enable_debug(void) {}
void disable_debug(void) {}

#endif  //  ARDUINO
