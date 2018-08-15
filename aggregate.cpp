//  Aggregate sensor data and decide whether to send to network.
#include "platform.h"
#include "sensor.h"
#include "wisol.h"
#include "aggregate.h"

#define maxSensorCount 3
static SensorMsg sensorData[maxSensorCount];

static const char testPayload[] = "0102030405060708090a0b0c";  //  TODO

void setup_aggregate(void) {
    //  Clear the aggregated sensor data.
    for (int i = 0; i < maxSensorCount; i++) {
        sensorData[i].name[0] = 0;  //  Clear the name.
        sensorData[i].count = 0;
    }
}

bool aggregate_sensor_data(
    WisolContext *context, 
    SensorMsg *msg, 
    WisolCmd cmdList[],
    int cmdListSize) {
    //  Aggregate the received sensor data.  Check whether we should send
    //  the data, based on the throttle settings.  Return true if
    //  we should send the message.  The message commands are
    //  populated in cmdList, up to cmdListSize elements (including the terminating command).

    //  Convert received sensor data to a list of Wisol commands.
    if (strncmp(context->msg->name, BEGIN_SENSOR_NAME, MAX_SENSOR_NAME_SIZE) == 0) {
        //  If sensor name is "000", this is the "begin" message.
        getCmdBegin(context, cmdList, cmdListSize);  //  Fetch list of startup commands for the transceiver.
        return true;  //  Send the startup commands.
    }

    //  TODO: Aggregate the sensor data.

    //  TODO: Throttle the sending.

    //  TODO: Encode the sensor data into a Sigfox message.

    //  Send the encoded Sigfox message.
    getCmdSend(context, cmdList, cmdListSize, testPayload, TEST_DOWNLINK);
    return true;
}
