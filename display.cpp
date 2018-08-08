#include <Arduino.h> ////
#include "sensor.h" ////
#include "display.h"
#include <string.h>
#include <cocoos.h>
#include <stdio.h>

//  Message buffer to be displayed at next refresh().
//  msg.name (sensor name) is unique in the array. If msg.count is 0, then msg is not used.
static DisplayMsg displayMessages[sensorDisplaySize];

static char buf[64];  //  Buffer to display 1 msg.
static char sensorBuf[32];  //  Buffer for sensor data for 1 msg.

static void refresh() {
  //  Refresh the display and show the sensor data.
  for (int i = 0; i < sensorDisplaySize; i++) {
    //  Compose each sensor msg and display it.
    DisplayMsg msg = displayMessages[i];    
    if (msg.count == 0) { continue; }
    sensorBuf[0] = 0;  //  Empty the buffer.
    for (int s = 0; s < msg.count && s < sensorDataSize; s++) {
      //  Merge the sensor values into a comma-separated string.
      float d = msg.data[s];  //  Given d = 12.3
      int16_t d1 = (int16_t) d;  //  Compute d1 = 12, d2 = 3.
      int16_t d2 = (d - d1) * 10;
      d2 = d2 % 10;
      sprintf(buf, "%d.%d", d1, d2);  //  e.g. 12.3
      if (s > 0) { strcat(sensorBuf, ",\t\t"); }  //  e.g. tmp: 12.3
      strcat(sensorBuf, buf);      
    }
    sprintf(buf, "%s:\t\t%s", msg.name, sensorBuf);
    displayMessages[i].count = 0;  //  Clear the sensor data.
    debug(buf);
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
    } else if (msg.count > 0 && strncmp(name, msg.name, sensorNameSize) == 0) {
      //  There is an existing msg with the same sensor name. Overwrite it.
      index = i;
      break;
    }
  }
  //  Use the index found or the first empty index.
  if (index < 0) { 
    index = firstEmptyIndex; 
    if (index < 0) {
      debug("Out of rows");  //  Need to increase sensorDisplaySize.
      return;
    }
  }
  //  Overwrite the message at the index.
  //  static char buf[128]; sprintf(buf, " %d, %d", index, count); debug("overwrite", buf); ////
  DisplayMsg *msgPtr = &displayMessages[index];
  memset(msgPtr->name, 0, sensorNameSize + 1);  //  Zero the name array.
  strncpy(msgPtr->name, name, sensorNameSize);  //  Set the sensor name e.g. tmp
  msgPtr->count = count;  //  Number of floats returned as sensor data.
  for (int i = 0; i < msgPtr->count && i < sensorDataSize; i++) {
    msgPtr->data[i] = data[i];
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
  //  Empty the display by setting the msg.count to 0.
  for (int i = 0; i < sensorDisplaySize; i++) {
    displayMessages[i].count = 0;
  }
}
