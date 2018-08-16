//  Aggregate sensor data and decide whether to send to network now.
#include "platform.h"
#include <string.h>
#include "display.h"
#include "sensor.h"
#include "wisol.h"
#include "aggregate.h"

static SensorMsg *recallSensor(const char *name);
static void copySensorData(SensorMsg *dest, SensorMsg *src);
static void addPayloadInt3(char *payloadBuffer, int payloadSize, const char *name, int data);

//  Remember the last sensor value of each sensor.
static SensorMsg sensorData[MAX_SENSOR_COUNT];

//  Buffer for constructing the message payload to be sent, in hex digits, plus terminating null.
#define PAYLOAD_SIZE (1 + MAX_MESSAGE_SIZE * 2)  //  Each byte takes 2 hex digits. Add 1 for terminating null.
static char payload[PAYLOAD_SIZE];  //  e.g. "0102030405060708090a0b0c"
//  static const char testPayload[] = "0102030405060708090a0b0c";  //  For testing

bool aggregate_sensor_data(
    WisolContext *context, 
    SensorMsg *msg, 
    WisolCmd cmdList[],
    int cmdListSize) {
    //  Aggregate the received sensor data.  Check whether we should send the data, based on 
    //  the throttle settings.  Return true if we should send the message.  The message commands are
    //  populated in cmdList, up to cmdListSize elements (including the terminating command).

    if (strncmp(msg->name, BEGIN_SENSOR_NAME, MAX_SENSOR_NAME_SIZE) == 0) {
        //  If sensor name is "000", this is the Begin Step.
        context->stepBeginFunc(context, cmdList, cmdListSize);  //  Fetch list of startup commands for the transceiver.
        return true;  //  Send the startup commands.
    }
    debug_print(msg->name); debug_print(F(" << Recv sensor data ")); 
    if (msg->count > 0) { debug_println(msg->data[0]); }
    else { debug_println("(empty)"); }
    debug_flush();

    //  Aggregate the sensor data.  Here we just save the last value for each sensor.
    SensorMsg *savedSensor = recallSensor(msg->name);
    if (savedSensor == NULL) return false;  //  Return error.
    copySensorData(savedSensor, msg);  //  Copy the data from the received message into the saved data.

    //  Throttle the sending.  TODO: Show warning if messages are sent faster than SEND_DELAY.
    unsigned long now = millis();
    if ((now - context->lastSend) < SEND_INTERVAL) { return false; }  //  Not ready to send.
    context->lastSend = now + MAX_TIMEOUT;  //  Prevent other requests from trying to send.

    //  Create a new Sigfox message. Add a running sequence number to the message.
    payload[0] = 0;  //  Empty the message payload.
    static int sequenceNumber = 0;
    addPayloadInt3(payload, PAYLOAD_SIZE, "seq", sequenceNumber++);

    //  Encode the sensor data into a Sigfox message.
    static const char *sendSensors[] = { "tmp", "hmd", "alt", NULL };  //  Sensors to be sent.
    for (int i = 0; sendSensors[i] != NULL; i++) {
        //  Get each sensor data and add to the message payload.
        float data = 0;
        const char *sensorName = sendSensors[i];
        savedSensor = recallSensor(sensorName);  //  Find the sensor.
        if (savedSensor != NULL) { data = savedSensor->data[0]; }  //  Fetch the sensor data (first float only).
        int scaledData = data * 10;  //  Scale up by 10 to send 1 decimal place. So 27.1 becomes 271
        addPayloadInt3(payload, PAYLOAD_SIZE, sensorName, scaledData);  //  Add to payload.
    }
    debug_print(F("agg >> Send ")); ////
    for (int i = 1; i < strlen(payload) && i < PAYLOAD_SIZE; i = i + 2) {
        debug_print(payload[i]);
    }
    debug_println(""); debug_flush();

    //  Compose the list of Wisol AT Commands for sending the message payload.
    context->stepSendFunc(context, cmdList, cmdListSize, payload, ENABLE_DOWNLINK);
    return true;  //  Will be sent by the caller.
}

static void addPayloadInt3(char *payloadBuffer, int payloadSize, const char *name, int data) {
    //  Add the integer data to the message payload as 3 ASCII digits in hexadecimal.
    //  So data=123 will be added as "313233".  Not efficient, but easy to read.
    int length = strlen(payloadBuffer);
    if (length + 6 >= payloadSize) {  //  No space for 3 ASCII digits (6 hex chars).
        debug(F("***** Error: No payload space for "), name);
        return;
    }
    if (data < 0 || data > 999) {  //  Show a warning if out of range.
        debug_print(F("***** Warning: Only last 3 digits of ")); 
        debug_print(name); debug_print(F(" value ")); debug_print(data);
        debug_println(" will be sent"); debug_flush();
    }
    for (int i = 2; i >= 0; i--) {  //  Add the 3 digits in reverse order (right to left).
        int d = data % 10;  //  Take the last digit.
        data = data / 10;  //  Shift to the next digit.
        payloadBuffer[length + (i * 2)] = '3';  //  Digits always start with hex 3 in ASCII.
        payloadBuffer[length + (i * 2) + 1] = '0' + d;  //  Write the digit to payload: 1 becomes '1'.
    }    
    payloadBuffer[length + 6] = 0;  //  Terminate the payload after adding 6 hex chars.
}

static void copySensorData(SensorMsg *dest, SensorMsg *src) {
    //  Copy sensor data from src to dest.
    for (int i = 0; i < src->count; i++) {
        dest->data[i] = src->data[i];
    }
    dest->count = src->count;
}

static SensorMsg *recallSensor(const char *name) {
    //  Return the sensor data for the sensor name.  If not found, allocate
    //  a new SensorMsg and return it.  If no more space, return NULL.
    int emptyIndex = -1;
    for (int i = 0; i < MAX_SENSOR_COUNT; i++) {
        //  Search for the sensor name in our data.
        if (strncmp(name, sensorData[i].name, MAX_SENSOR_NAME_SIZE) == 0) {
            return &sensorData[i];  //  Found it.
        }
        //  Find the first empty element.
        if (emptyIndex == -1 && sensorData[i].name[0] == 0) {
            emptyIndex = i;
        }
    }
    //  Allocate a new element.
    if (emptyIndex == -1) {  //  No more space.
        debug(F("***** Error: No aggregate space for "), name);
        return NULL;
    }
    strncpy(sensorData[emptyIndex].name, name, MAX_SENSOR_NAME_SIZE);
    sensorData[emptyIndex].name[MAX_SENSOR_NAME_SIZE] = 0;  //  Terminate the sensor name in case of overflow.
    sensorData[emptyIndex].count = 0;  //  Clear the values.
    sensorData[emptyIndex].data[0] = 0;  //  Reset to 0 in case we need to send.
    return &sensorData[emptyIndex];
}

void setup_aggregate(void) {
    //  Clear the aggregated sensor data.
    for (int i = 0; i < MAX_SENSOR_COUNT; i++) {
        sensorData[i].name[0] = 0;  //  Clear the name.
        sensorData[i].count = 0;  //  Clear the values.
    }
}
