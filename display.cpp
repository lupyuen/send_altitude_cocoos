#include <Arduino.h> ////
#include "sensor.h" ////
#include "display.h"
#include <string.h>
#include <cocoos.h>
#include <stdio.h>

//  TODO: This overrides OS_MSG_Q_RECEIVE macro defined in cocoOS_5.0.1/src/os_msgqueue.h.
//  This is a workaround for the cross initialisation problem when used in C++ code.
//  Caller function must declare the following before task_open():
//    MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.
//  The only changes are in these lines:
//    /* MsgQ_t */ queue = os_msgQ_find(task_id);\
//    /* Evt_t */ event = os_msgQ_event_get(queue);\

#define OS_MSG_Q_RECEIVE(task_id, pMsg, async)     do {\
                                                    uint8_t os_received;\
                                                    /* MsgQ_t */ queue = os_msgQ_find(task_id);\
                                                    os_task_set_wait_queue(running_tid, queue);\
                                                    /* Evt_t */ event = os_msgQ_event_get(queue);\
                                                    os_task_set_change_event(running_tid, event);\
                                                    do {\
                                                        os_received = os_msg_receive((Msg_t*)pMsg, os_msgQ_find(task_id));\
                                                        if ( os_received == MSG_QUEUE_EMPTY ){\
                                                            if ( async == 0 ) {\
       	                                                        os_task_set_msg_result(running_tid, os_received);\
                                                                event_wait(event);\
                                                                os_received = os_task_get_msg_result(running_tid);\
                                                                event = os_task_get_change_event(running_tid);\
                                                            }\
                                                            else {\
                                                                ((Msg_t*)pMsg)->signal = NO_MSG_ID;\
                                                                os_received = MSG_QUEUE_UNDEF;\
                                                            }\
                                                        }\
                                                    } while ( os_received == MSG_QUEUE_EMPTY );\
                                                    if ( MSG_QUEUE_RECEIVED == os_received) {\
                                                    	os_signal_event(event);\
                                                    	os_event_set_signaling_tid(event, running_tid );\
													}\
                                                } while(0)

//  Message buffer to be displayed at next refresh().
//  msg.name (sensor name) is unique in the array. If msg.count is 0, then msg is not used.
static DisplayMsg displayMessages[sensorDisplaySize];

static char buf[64];  //  Buffer to display 1 msg.
static char sensorBuf[32];  //  Buffer for sensor data for 1 msg.

void display_task(void) {
  //  Background task that receives display messages and displays them.
  static DisplayMsg msg;
  MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.
  task_open();
  for (;;) {
    //  Wait for an incoming display message containing sensor data.
    //// debug("msg_receive", 0); ////

    //  Note for Arduino: msg_receive() must be called in a C source file, not C++.
    //  The macro expansion fails in C++ with a cross-initialisation error.
    msg_receive(os_get_running_tid(), &msg);
    Display *display = (Display *) task_get_data();

    //  Update the sensor data to be displayed.
    //// debug(msg.name, " >> updateData"); ////
    display->update_data_func(msg.super.signal, msg.name, msg.data, msg.count);

    //  Display the sensor data.
    //// debug(msg.name, " >> refresh"); ////
    display->refresh_func();
  }
  task_close();
}

static void refresh(void) {
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

Display *get_display(void) {
  return &display;
}

void init_display(void) {
  //  Empty the display by setting the msg.count to 0.
  for (int i = 0; i < sensorDisplaySize; i++) {
    displayMessages[i].count = 0;
  }
}
