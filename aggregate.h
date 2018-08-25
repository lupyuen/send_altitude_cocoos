#ifndef AGGREGATE_H_
#define AGGREGATE_H_
#include "platform.h"

// #define ENABLE_DOWNLINK false  //  Uplink data only
#define ENABLE_DOWNLINK true  //  Uplink data and request for downlink



BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

struct NetworkContext;  //  Forward declaration
struct NetworkCmd;  //  Forward declaration
struct SensorMsg;  //  Forward declaration

void setup_aggregate(void);   //  Init the sensor data aggregator.
bool aggregate_sensor_data(   //  Aggregate the received sensor data. Return true if we should send to network now.
    NetworkContext *context,  //  Context storage for the Network Task.
    SensorMsg *msg,           //  Sensor Data Message just received. Contains sensor name and sensor values.
    NetworkCmd cmdList[],     //  Upon return, will be populated by the list of AT Commands to be executed.
    int cmdListSize);         //  How many commands may be stored in cmdList.

void aggregate_getSensorData(char *buf);
void aggregate_saveSensorData(SensorMsg *msg);

END_EXTERN_C  //  End of extern C scope.

#endif  //  AGGREGATE_H_
