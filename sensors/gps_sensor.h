#ifndef GPS_SENSOR_H_
#define GPS_SENSOR_H_

#include "sensor.h"

#ifdef __cplusplus
extern "C" {
#endif

SensorContext *setup_gps_sensor(uint16_t pollInterval, uint8_t aggregator_id);

#ifdef __cplusplus
}

#endif

#endif /* GPS_SENSOR_H_ */
