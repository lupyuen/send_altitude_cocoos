#include <Arduino.h> ////
#include "sensor.h" ////
#include "display.h"
#include <string.h>
#include <cocoos.h>
#include <stdio.h>

//  Message buffer for display task
DisplayMsg displayMessages[10];

//  Store updated sensor data locally before display.
static float _temp = 0;
static uint8_t _x = 0;
static uint8_t _y = 0;
static uint8_t _z = 0;

static char buf[256]; //  Resulting string limited to 256 chars.

static void serial_printf(const char fmt[], long a1 = 0, long a2 = 0, long a3 = 0, long a4 = 0, long a5 = 0) {
  sprintf(buf, fmt, a1, a2, a3, a4, a5);
  debug(buf);
}

static void refresh() {
  //  Refresh the display and show the sensor data.
  //// debug("refresh"); ////
  if (_temp != 0) {
    serial_printf("Temp:\t\t%ld", _temp); ////
    _temp = 0;
  }
  if (_x != 0 || _y != 0 || _z != 0) {
    serial_printf("Gyro:\t\tx:%ld\t\ty:%ld\t\tz:%ld", _x, _y, _z); ////
    _x = 0; _y = 0; _z = 0;    
  }
}

static void updateData(uint8_t id, const float *data, uint8_t count) {
  //  Save the updated sensor data for display later.
  //// debug("updateData"); ////
  switch(id) {
    case TEMP_DATA:
      if (count >= 1) {
        _temp = data[0];
      }
      break;

    case GYRO_DATA:
      if (count >= 3) {
        _x = data[0];
        _y = data[1];
        _z = data[2];
      }
      break;

    default:
      debug("Bad ID");
  }
}

static Display display = {
  .refresh_func = &refresh,
  .update_data_func = &updateData
};

Display *display_get(void) {
  return &display;
}

void display_init(void) {
}
