#include "display.h"
#include <string.h>
#include <cocoos.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>


// Message buffer for display task
DisplayMsg_t displayMessages[10];

static const char *tempdata;
static uint8_t _x = 1;
static uint8_t _y = 2;
static uint8_t _z = 3;


static void update() {
  // clear current line and move to start of line
  printf("%c" "%s" "%c", ESC, CLEAR_LINE, CR);

  printf("-----------------Sensor readings--------------------");

  // move to next line and print temp data
  printf("%c" "%s" "%c" , ESC, MOVE_DOWN, CR);
  printf("%c%s%s",ESC, CLEAR_LINE, tempdata);

  // move to next line and print gyro data
  printf("%c" "%s" "%c" , ESC, MOVE_DOWN, CR);
  printf("%c%sGyro:\t\tx:%d\ty:%d\tz:%d",ESC, CLEAR_LINE, _x, _y, _z);

  // go home
  printf("%c" "%s" "%c" , ESC, MOVE_UP(2), CR);

  fflush(stdout);
}

static void updateData(uint8_t id, const char *data) {
  if (id == TEMP_DATA) {
    tempdata = data;
  }
  else if (id == GYRO_DATA) {
    _x = data[0];
    _y = data[1];
    _z = data[2];
  }
}
static Display_t display = {
    .update = &update,
    .updateData = &updateData
};

Display_t *display_get(void) {
  return &display;
}

void display_init(void) {

  struct termios old, new;
  tcgetattr(STDIN_FILENO, &old);          // get current settings
  new = old;                              // create a backup
  new.c_lflag &= ~(ICANON | ECHO);        // disable line buffering and feedback
  tcsetattr(STDIN_FILENO, TCSANOW, &new); // set our new config

  printf("\n\n\n");
  printf("\n\n\n");
  printf("\n\n\n");
  printf("%c" "%s" , ESC, MOVE_UP(7));

  printf("%c" "%s" , ESC, MOVE_DOWN);
}


