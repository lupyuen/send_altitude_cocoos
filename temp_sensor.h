#ifndef TEMP_SENSOR_H_
#define TEMP_SENSOR_H_

#include "sensor.h"

Sensor_t *tempSensor_get(void);
void tempSensor_service(void);

#endif /* TEMP_SENSOR_H_ */
