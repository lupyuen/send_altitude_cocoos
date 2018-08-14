//  Implements the Display Task that receives display messages and displays them.
#include "platform.h"
#include <string.h>
#include <stdio.h>
#include "cocoos_cpp.h"  //  TODO: Workaround for cocoOS in C++
#include "sensor.h"
#include "display.h"

#ifdef SENSOR_DISPLAY
//  Message buffer to be displayed at next refresh().
//  msg.name (sensor name) is unique in the array. If msg.count is 0, then msg is not used.
static DisplayMsg displayMessages[sensorDisplaySize];

static char buf[64];  //  Buffer to display 1 msg.
static char sensorBuf[32];  //  Buffer for sensor data for 1 msg.

void display_task(void) {
  //  Background task that receives display messages and displays them.
  static DisplayMsg msg;
  //  Assume only 1 display task runnning, so the following can be made static.
  static MsgQ_t queue; static Evt_t event;  //  TODO: Workaround for msg_receive() in C++.
  task_open();  //  Start of the task. Must be matched with task_close().
  for (;;) { //  Run the display processing code forever. So the task never ends.
    //  Wait for an incoming display message containing sensor data.
    //// debug(F("msg_receive")); ////
    msg_receive(os_get_running_tid(), &msg);
    static Display *display = (Display *) task_get_data();  //  Only 1 display task running.

    //  Update the sensor data to be displayed.
    //// debug(msg.name, F(" >> updateData")); ////
    display->update_data_func(msg.super.signal, msg.name, msg.data, msg.count);

    //  Display the sensor data.
    //// debug(msg.name, F(" >> refresh")); ////
    display->refresh_func();
  }
  task_close();  //  End of the task. Should never come here.
}

static void refresh(void) {
  //  Refresh the display and show the sensor data.
  for (int i = 0; i < sensorDisplaySize; i++) {
    //  Compose each sensor msg and display it e.g. tmp: 12.3, 12.4
    DisplayMsg msg = displayMessages[i];    
    if (msg.count == 0) { continue; }
    sensorBuf[0] = 0;  //  Empty the buffer.
    for (int s = 0; s < msg.count && s < maxSensorDataSize; s++) {
      //  Merge the sensor values into a comma-separated string e.g. 12.3, 12.4
      float d = msg.data[s];  //  Given d = 12.3
      int16_t d1 = (int16_t) d;  //  Compute d1 = 12, d2 = 3.
      int16_t d2 = (d - d1) * 10;
      d2 = d2 % 10;
      //  Implement sprintf(buf, "%d.%d", d1, d2);  //  e.g. 12.3
      buf[0] = 0;
      itoa(d1, buf + strlen(buf), 10);
      strncat(buf, ".", sizeof(buf) - strlen(buf) - 1);
      buf[strlen(buf) - 1] = 0;  //  Terminate buf in case of overflow.
      itoa(d2, buf + strlen(buf), 10);
      //  Separate values with tab.
      if (s > 0) { strcat(sensorBuf, ",\t\t"); }  //  e.g. 12.3, 12.4
      strcat(sensorBuf, buf);      
    }
    //  Implement sprintf(buf, "%s:\t\t%s", msg.name, sensorBuf);  //  e.g. tmp: 12.3, 12.4
    buf[0] = 0;
    strncat(buf, msg.name, sizeof(buf) - strlen(buf) - 1);
    buf[strlen(buf) - 1] = 0;  //  Terminate buf in case of overflow.
    strncat(buf, ":\t\t", sizeof(buf) - strlen(buf) - 1);
    buf[strlen(buf) - 1] = 0;  //  Terminate buf in case of overflow.
    strncat(buf, sensorBuf, sizeof(buf) - strlen(buf) - 1);
    buf[strlen(buf) - 1] = 0;  //  Terminate buf in case of overflow.

    displayMessages[i].count = 0;  //  Clear the sensor data.
    debug(buf);  //  Display the line.
  }
}

static void updateData(uint8_t id, const char *name, const float *data, uint8_t count) {
  //  Save the updated sensor data for display later.
  int index = -1;  //  Index to overwrite msg.
  int firstEmptyIndex = -1;  //  Index of the first empty msg.
  for (int i = 0; i < sensorDisplaySize; i++) {
    DisplayMsg msg = displayMessages[i];  //  Note: This clones the message.
    if (msg.count == 0 && firstEmptyIndex < 0) {
      //  Look for the first empty message.
      firstEmptyIndex = i;
    } else if (msg.count > 0 && strncmp(name, msg.name, maxSensorNameSize) == 0) {
      //  There is an existing msg with the same sensor name. Overwrite it.
      index = i;
      break;
    }
  }
  //  Use the index found or the first empty index.
  if (index < 0) { 
    index = firstEmptyIndex; 
    if (index < 0) {
      debug(F("Out of rows"));  //  Need to increase sensorDisplaySize.
      return;
    }
  }
  //  Overwrite the message at the index.
  //  static char buf[128]; sprintf(buf, " %d, %d", index, count); debug("overwrite", buf); ////
  DisplayMsg *msgPtr = &displayMessages[index];
  //// memset(msgPtr->name, 0, maxSensorNameSize + 1);  //  Zero the name array.
  strncpy(msgPtr->name, name, maxSensorNameSize);  //  Set the sensor name e.g. tmp
  msgPtr->name[maxSensorNameSize] = 0;  //  Terminate the name in case of overflow.
  msgPtr->count = count;  //  Number of floats returned as sensor data.
  for (int i = 0; i < msgPtr->count && i < maxSensorDataSize; i++) {
    msgPtr->data[i] = data[i];
  }
}

void init_display(void) {
  //  Empty the display by setting the msg.count to 0.
  for (int i = 0; i < sensorDisplaySize; i++) {
    displayMessages[i].count = 0;
  }
}

//  Construct the global display object.
static Display display = {
  .refresh_func = &refresh,
  .update_data_func = &updateData
};

Display *get_display(void) {
  //  Return the global instance of the display object.
  return &display;
}

#endif  //  SENSOR_DISPLAY

//  Print a message to the Arduino serial console.  The function is overloaded to support
//  printing of strings in dynamic memory and strings in flash (e.g. F(...)).

void debug(const char *s1, const char *s2) {
  //  Print 2 dynamics strings.
  Serial.begin(SERIAL_BAUD);
  while (!Serial) {}  //  Wait for Serial to be ready.
  Serial.print(s1);
  if (s2) Serial.print(s2);
  Serial.println("");
  Serial.flush();  //  Let serial printing finish.
}

void debug(const __FlashStringHelper *s1) {
  //  Print 1 flash string.
  Serial.begin(SERIAL_BAUD);
  while (!Serial) {}  //  Wait for Serial to be ready.
  Serial.print(s1);
  // if (s2) Serial.print(s2);
  Serial.println("");
  Serial.flush();  //  Let serial printing finish.
}

void debug(const char *s1, const __FlashStringHelper *s2) {
  //  Print 1 dynamic string and 1 flash string.
  Serial.begin(SERIAL_BAUD);
  while (!Serial) {}  //  Wait for Serial to be ready.
  Serial.print(s1);
  if (s2) Serial.print(s2);
  Serial.println("");
  Serial.flush();  //  Let serial printing finish.
}

void debug(const __FlashStringHelper *s1, const char *s2) {
  //  Print 1 flash string and 1 dynamic string.
  Serial.begin(SERIAL_BAUD);
  while (!Serial) {}  //  Wait for Serial to be ready.
  Serial.print(s1);
  if (s2) Serial.print(s2);
  Serial.println("");
  Serial.flush();  //  Let serial printing finish.
}
