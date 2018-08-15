#ifndef AGGREGATE_H_
#define AGGREGATE_H_

// #define ENABLE_DOWNLINK false  //  Uplink data only
#define ENABLE_DOWNLINK true  //  Uplink data and request for downlink

//  Send a sensor message every 20,000 milliseconds = 20 seconds.
#define SEND_INTERVAL ((unsigned long) 20 * 1000)

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

struct WisolContext;  //  Forward declaration
struct WisolCmd;  //  Forward declaration
struct SensorMsg;  //  Forward declaration

void setup_aggregate(void);
bool aggregate_sensor_data(
    WisolContext *context, 
    SensorMsg *msg, 
    WisolCmd cmdList[],
    int cmdListSize);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  AGGREGATE_H_
