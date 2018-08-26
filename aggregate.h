#ifndef AGGREGATE_H_
#define AGGREGATE_H_
#include "platform.h"

// #define ENABLE_DOWNLINK false  //  Uplink data only
#define ENABLE_DOWNLINK true  //  Uplink data and request for downlink



BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.

struct SensorMsg;  //  Forward declaration

void setup_aggregate(void);   //  Init the sensor data aggregator.
void aggregate_getSensorData(char *buf);
void aggregate_saveSensorData(const SensorMsg *msg);

END_EXTERN_C  //  End of extern C scope.

#endif  //  AGGREGATE_H_
