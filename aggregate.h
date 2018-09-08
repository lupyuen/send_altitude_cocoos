#ifndef AGGREGATE_H_
#define AGGREGATE_H_
#include "network.h"
#include "platform.h"
#include "sensor.h"

// #define ENABLE_DOWNLINK false  //  Uplink data only
#define ENABLE_DOWNLINK true  //  Uplink data and request for downlink



BEGIN_EXTERN_C  //  Allows functions below to be called by C and C++ code.


struct AggregateContext {
  SensorMsg periodicMsg;            // Message to itself
  NetworkMsg  networkMsg;           // Outgoing radio message
  uint32_t sendPeriodInSeconds;     // How often we will send to radio
  uint8_t networkTaskID;            // Task that aggregate task will send messages to
};

void setup_aggregate();

void aggregate_task(void);

END_EXTERN_C  //  End of extern C scope.

#endif  //  AGGREGATE_H_
