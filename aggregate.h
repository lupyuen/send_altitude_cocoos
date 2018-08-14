#ifndef AGGREGATE_H_
#define AGGREGATE_H_

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

struct WisolContext;  //  Forward declaration
struct SensorMsg;  //  Forward declaration

void setup_aggregate(void);
bool aggregate_sensor_data(WisolContext *context, SensorMsg *msg);

#ifdef __cplusplus
}  //  End of extern C scope.
#endif
#endif  //  AGGREGATE_H_
