#include <Arduino.h> ////
#include "sensor.h" ////
#include "display.h"
#include <string.h>
#include <cocoos.h>
#include <stdio.h>

// Message buffer for display task
DisplayMsg displayMessages[10];

static const char *tempdata;
static float _temp = 0;
static uint8_t _x = 0;
static uint8_t _y = 0;
static uint8_t _z = 0;

/////
#include <stdarg.h>
static char buf[256]; // resulting string limited to 256 chars
static void serial_printf(const char fmt[], long a1 = 0, long a2 = 0, long a3 = 0, long a4 = 0, long a5 = 0) {
  sprintf(buf, fmt, a1, a2, a3, a4, a5);
  debug(buf);
}
#define printf serial_printf
////

static void refresh() {
  if (tempdata != NULL) {
    debug(tempdata); ////
    tempdata = NULL;
  }
  if (_x != 0 || _y != 0 || _z != 0) {
    ////printf("%c%sGyro:\t\tx:%d\ty:%d\tz:%d",ESC, CLEAR_LINE, _x, _y, _z);
    printf("Gyro:\t\tx:%ld\ty:%ld\tz:%ld", _x, _y, _z); ////
    _x = 0; _y = 0; _z = 0;    
  }
}

static void updateData(uint8_t id, const float *data, uint8_t count) {
  if (id == TEMP_DATA) {
    if (count >= 1) _temp = data[0];
  } else if (id == GYRO_DATA) {
    if (count >= 3) {
      _x = data[0];
      _y = data[1];
      _z = data[2];
    }
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
