#ifndef DISPLAY_H_
#define DISPLAY_H_

#include <stdint.h>
#include "cocoos.h"

#ifdef __cplusplus ////
extern "C" {
#endif ////

#define ESC 27
#define CR  0xd
#define CLEAR_LINE "[2K"
#define MOVE_DOWN "[1B"
#define MOVE_UP(n) "["#n"A"

// Message signals
#define TEMP_DATA 32
#define GYRO_DATA 33
#define DISPLAY_MSG 34

/**
 * Message type for display task
 * The value of super.signal indicates which type of data,
 * (temp_sensor or gyro data in this case)
 */
typedef struct {
  Msg_t super;
  const float *data;
  uint8_t count;
} DisplayMsg;

typedef struct {
  void (*refresh_func)(void);
  void (*update_data_func)(uint8_t id, const float *data, uint8_t count);
} Display;

Display *display_get(void);
void display_init(void);

extern DisplayMsg displayMessages[10];

#ifdef __cplusplus ////
}
#endif ////

#endif /* DISPLAY_H_ */
