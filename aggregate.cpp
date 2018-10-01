//  Aggregate sensor data and decide whether to send to network now.
#include "platform.h"
#include <string.h>
#include "display.h"
#include "sensor.h"
#include "wisol.h"
#include "aggregate.h"

static SensorMsg *recallSensor(const char *name);
static void copySensorData(SensorMsg *dest, SensorMsg *src);
static void addPayloadInt(
    char *payloadBuffer, 
    int payloadSize, 
    const char *name, 
    int data,
    int numDigits,
    int maxNumber);

//  Remember the last sensor value of each sensor.
static SensorMsg sensorData[MAX_SENSOR_COUNT];

#define NUM_DIGITS 4          //  Number of digits to be sent for each sensor value.
#define MAX_NUMBER (1E4 - 1)  //  Max sensor value that can be sent (after scaling).  Depends on NUM_DIGITS.

//  Buffer for constructing the message payload to be sent, in hex digits, plus terminating null.
#define PAYLOAD_SIZE (1 + MAX_MESSAGE_SIZE * 2)  //  Each byte takes 2 hex digits. Add 1 for terminating null.
static char payload[PAYLOAD_SIZE];  //  e.g. "0102030405060708090a0b0c"
//  static const char testPayload[] = "0102030405060708090a0b0c";  //  For testing

bool aggregate_sensor_data(
    NetworkContext *context,  //  Context storage for the Network Task.
    SensorMsg *msg,           //  Sensor Data Message just received. Contains sensor name and sensor values.
    NetworkCmd cmdList[],     //  Upon return, will be populated by the list of AT Commands to be executed.
    int cmdListSize) {        //  How many commands may be stored in cmdList.
    //  Aggregate the received sensor data.  Check whether we should send the data, based on 
    //  the throttle settings.  Return true if we should send the message.  The message commands are
    //  populated in cmdList, up to cmdListSize elements (including the terminating command).
    led_toggle();  //  Blink the LED so we know we are aggregating continuously.
    if (strncmp(msg->name, BEGIN_SENSOR_NAME, MAX_SENSOR_NAME_SIZE) == 0) {
        //  If sensor name is "000", this is the Begin Step.
        context->stepBeginFunc(context, cmdList, cmdListSize);  //  Fetch list of startup commands for the transceiver.
        return true;  //  Send the startup commands.
    }
    debug_print(msg->name); debug_print(F(" << Recv data ")); 
    if (msg->count > 0) { debug_println(msg->data[0]); }
    else { debug_println("(empty)"); }
    //  debug_flush();

    //  Aggregate the sensor data.  Here we just save the last value for each sensor.
    SensorMsg *savedSensor = recallSensor(msg->name);
    if (savedSensor == NULL) return false;  //  Return error.
    copySensorData(savedSensor, msg);  //  Copy the data from the received message into the saved data.

    //  Throttle the sending.  TODO: Show warning if messages are sent faster than SEND_DELAY.
    unsigned long now = millis();
    if ((context->lastSend + SEND_INTERVAL) > now) { return false; }  //  Not ready to send.
    context->lastSend = now + MAX_TIMEOUT;  //  Prevent other requests from trying to send.

    //  Create a new Sigfox message. Add a running sequence number to the message.
    payload[0] = 0;  //  Empty the message payload.
    static int sequenceNumber = 0;
    addPayloadInt(payload, PAYLOAD_SIZE, "seq", sequenceNumber++, NUM_DIGITS, MAX_NUMBER);

    //  Encode the sensor data into a Sigfox message, 4 digits each.
    static const char *sendSensors[] = { "tmp", "hmd", "alt", NULL };  //  Sensors to be sent.
    for (int i = 0; sendSensors[i] != NULL; i++) {
        //  Get each sensor data and add to the message payload.
        float data = 0;
        const char *sensorName = sendSensors[i];
        savedSensor = recallSensor(sensorName);  //  Find the sensor.
        if (savedSensor != NULL) { data = savedSensor->data[0]; }  //  Fetch the sensor data (first float only).
        int scaledData = data * 10;  //  Scale up by 10 to send 1 decimal place. So 27.1 becomes 271
        addPayloadInt(payload, PAYLOAD_SIZE, sensorName, scaledData, NUM_DIGITS, MAX_NUMBER);  //  Add to payload.
    }
    //  If the payload has odd number of digits, pad with '0'.
    int length = strlen(payload);
    if (length % 2 != 0 && length < PAYLOAD_SIZE - 1) {
        payload[length] = '0';
        payload[length + 1] = 0;
    }
    debug_print(F("agg >> Send ")); debug_println(payload);

    //  Compose the list of Wisol AT Commands for sending the message payload.
    context->stepSendFunc(context, cmdList, cmdListSize, payload, ENABLE_DOWNLINK);
    return true;  //  Will be sent by the caller.
}

static void addPayloadInt(
    char *payloadBuffer, 
    int payloadSize, 
    const char *name, 
    int data,
    int numDigits,
    int maxNumber) {
    //  Add the integer data to the message payload as numDigits digits in hexadecimal.
    //  So data=1234 and numDigits=4, it will be added as "1234".  Not efficient, but easy to read.
    //  maxNumber=pow(10, numDigits) - 1.  This prevents linker from adding the pow() from the standard
    //  C math library that occupies 2.7 KB! 
    int length = strlen(payloadBuffer);
    if (length + numDigits >= payloadSize) {  //  No space for numDigits hex digits.
        debug(F("***** Error: No payload space for "), name);
        return;
    }
    if (data < 0 || data > maxNumber) {  //  Show a warning if out of range.
        debug_print(F("***** Warning: Only last ")); debug_print(numDigits); 
        debug_print(F(" digits of ")); debug_print(name); debug_print(F(" value ")); debug_print(data);
        debug_println(" will be sent"); // debug_flush();
    }
    for (int i = numDigits - 1; i >= 0; i--) {  //  Add the digits in reverse order (right to left).
        int d = data % 10;  //  Take the last digit.
        data = data / 10;  //  Shift to the next digit.
        payloadBuffer[length + i] = '0' + d;  //  Write the digit to payload: 1 becomes '1'.
    }
    payloadBuffer[length + numDigits] = 0;  //  Terminate the payload after adding numDigits hex chars.
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
