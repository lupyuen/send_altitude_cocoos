//  Functions to send and receive data from the UART serial port, e.g. for Wisol module.
#include "network.h"
#include "radio.h"
#include "platform.h"
#include <string.h>
#include <cocoos.h>
#include "sensor.h"
#include "downlink.h"
#include "utils.h"


static void serialize(NetworkCmd *cmd, char *buf);
static void processResponse(NetworkContext *context);

static NetworkCmd cmdList[MAX_NETWORK_CMD_LIST_SIZE];  //  Static buffer for storing command list. Includes terminating msg.
NetworkCmd endOfList = { NULL, NULL, NULL, NULL, 0 };  //  Command to indicate end of command list.

#define END_OF_RESPONSE '\r'  //  Character '\r' marks the end of response.
#define CMD_END "\r"

void network_task(void) {
  //  This task loops and waits for an incoming message containing sensor data to be sent.
  //  The sensorData array contains float values of sensor readings which are converted to
  //  a string of hex digits and is added to a string of commands.
  //  We send the resulting string to the radio. The radio is setup to generate an rxDone
  //  event when the expected count of markers ('\r') has been received.

  NetworkContext *context;     //  The context for the task.
  static NetworkMsg msg;       //  The received message.


  task_open();

  context = (NetworkContext *) task_get_data();
  context->rxDoneEvent = event_create();
  context->radio->setDoneEvent(context->rxDoneEvent);

  for (;;) {
    msg_receive(os_get_running_tid(), &msg);

    context = (NetworkContext *) task_get_data();  //  Must fetch again after msg_receive().

    // Empty the command list
    context->nCommands = 0;
    context->cmd = &cmdList[0];
    cmdList[0] = endOfList;

    // Fetch the commands to send from the radio driver implementation
    if (!context->initialized) {
      context->nCommands = context->radio->getStepBegin(cmdList, MAX_NETWORK_CMD_LIST_SIZE, false);
    }
    else {
      // perform step send
      context->nCommands = context->radio->getStepSend(context, cmdList, MAX_NETWORK_CMD_LIST_SIZE, &msg.sensorData[0], false);
    }

    //  Assume the return status will be successful.
    context->status = true;

    //  Send each AT command in the list.
    while (context->nCommands) {
      context = (NetworkContext *) task_get_data();  //  Must get context to be safe.

      if (context->cmd->sendData == NULL) { break; }  //  No more commands to send.

      //  Convert network command to serialized data
      char serializedSendData[MAX_RADIO_SEND_MSG_SIZE + 1];
      serialize(context->cmd, serializedSendData);

      // setup response logic : request event when expected number of markers have been received
      context->radio->setMarkerCount(context->cmd->expectedMarkerCount);

      context->radio->send((const uint8_t*)serializedSendData, strlen(serializedSendData));

      // wait for event or timeout
      event_wait_timeout(context->rxDoneEvent, 10000);

      context = (NetworkContext *) task_get_data();

      // was it timeout or rx done?
      if (event_get_timeout() == 0) {
          //timeout abort sending commands
          break;
      }

      // fetch received message(s)
      uint8_t len;
      len = context->radio->receive((uint8_t*)context->response);

      // make sure we null terminate
      context->response[len] = '\0';

      processResponse(context);  //  Process the response by calling the response function.

      if (context->status == false) break;  //  Quit if the processing failed.

      if (!context->initialized) {
        context->initialized = true;
      }

      context->cmd++;
      context->nCommands--;
    }



    context = (NetworkContext *) task_get_data();

  }  //  Loop back and wait for next queued message.

  task_close();  //  End of the task. Should not come here.
}

static void serialize(NetworkCmd *cmd, char *buf) {
  buf[0] = 0;

  if (cmd->sendData) {
    strncpy(buf, cmd->sendData, MAX_RADIO_SEND_MSG_SIZE - strlen(buf));
  }

  if (cmd->payload) {
    strncat(buf, cmd->payload, MAX_RADIO_SEND_MSG_SIZE - strlen(buf));
  }

  if (cmd->sendData2) {
    strncat(buf, cmd->sendData2, MAX_RADIO_SEND_MSG_SIZE - strlen(buf));
  }

  //  Terminate the command with "\r".
  strncat(buf, CMD_END, MAX_RADIO_SEND_MSG_SIZE - strlen(buf));
  buf[MAX_RADIO_SEND_MSG_SIZE] = 0;  //  Terminate the UART data in case of overflow.
}


static void processResponse(NetworkContext *context) {
  //  Process the response from the Wisol AT Command by calling the
  //  process response function.  Set the status to false if the processing failed.

  //  Get the response text.
  const char *response = (context && context->response)
    ? context->response
    : "";
  //  Process the response text, regardless of success/failure.
  //  Call the process response function if has been set.
  if (context->cmd->processFunc) {
    bool processStatus = (context->cmd->processFunc)(context, response);
    //  If response processing failed, stop.
    if (processStatus != true) {
      context->status = false;  //  Propagate status to Wisol context.
      return;  //  Quit processing.
    }

    if (context->downlinkData) {
      process_downlink_msg(context, context->status, context->downlinkData);
    }
  }

}
