//  Functions to send and receive data from the UART serial port, e.g. for Wisol module.
#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "radio.h"
#include "sensor.h"


void radio_task(void) {
  //  This task loops and waits for an incoming message containing UART data to be sent.
  //  sendData contains a string of ASCII chars to be sent to the UART port.
  //  We send the sendData to the port.  expectedMarkerCount is the number of
  //  end-of-command markers '\r' we expect to see.  actualMarkerCount
  //  contains the actual number seen. We trigger to the caller the events successEvent or failureEvent
  //  depending on success/failure sending the data.  Response is recorded in the
  //  "response" variable of the context, for the caller to retrieve.

  RadioContext *context;     //  The context for the task.
  static RadioMsg msg;       //  The received message.

  task_open();

  context = (RadioContext *) task_get_data();
  context->rxDoneEvent = event_create();
  context->radio->setDoneEvent(context->rxDoneEvent);

  for (;;) {
    msg_receive(os_get_running_tid(), &msg);

    context = (RadioContext *) task_get_data();  //  Must fetch again after msg_receive().
    context->msg = &msg;

    //  Initialise the context for the task. These variables will change while sending.
    context->status = true;         //  Assume the return status will be successful.

    // setup response logic : request event when expected number of markers have been received
    context->radio->setMarkerCount(context->msg->expectedMarkerCount);

    //  Send message
    context->radio->send((const uint8_t*)context->msg->sendData, strlen(context->msg->sendData));

    // wait for event or timeout
    event_wait_timeout(context->rxDoneEvent, context->msg->timeout);

    context = (RadioContext *) task_get_data();

    // was it timeout or rx done?
    if (event_get_timeout() == 0) {
        //timeout
        context->status = false;
    }

    // fetch received message(s)
    uint8_t len;
    len = context->radio->receive((uint8_t*)context->response);

    // make sure we null terminate
    context->response[len] = '\0';

    if (context->msg->responseMsg != NULL) {
      //  If caller has requested for response message, then send it instead of event.
      msg_post(context->msg->responseTaskID, *(context->msg->responseMsg));

    } else if (context->status == true) {
      //  If no error, trigger the success event to caller.
      //  The caller can read the response from the context.response.
      event_signal(context->msg->successEvent);
    } else {
      //  If we hit an error, trigger the failure event to the caller.
      event_signal(context->msg->failureEvent);
    }

    context = (RadioContext *) task_get_data();

  }  //  Loop back and wait for next queued message.

  task_close();  //  End of the task. Should not come here.
}


