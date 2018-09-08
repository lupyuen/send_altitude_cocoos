
#include <cocoos.h>
#include "sensor.h"
#include "gps_sensor.h"
#include "uartSerial.h"
#include "config.h"
#include "string.h"

static void init(void);
static uint8_t poll(float *data, uint8_t size);
static bool parse(char *nmea);

//  Construct a sensor object with the sensor functions.
static Sensor sensor(
  "gps",                //  Name of sensor. The Structured Message field will use this name.
  &init,         //  Function for initialising the sensor.
  &poll          //  Function for polling sensor data.
);

static SensorContext sensorContext;

static uint8_t nmeainput[128];
static bool bufferOpen = true;

static float latitude;
static float longitude;
static float latitudeDegrees = 57.6345030;
static float longitudeDegrees = 11.9578160;


class GpsReceiver: public UartSerial::IReaderCb  {
public:
  GpsReceiver(UartSerial *serial, uint8_t *buf): dev(serial), rxbuf(buf), writepos(0) {
    dev->registerReader(this);
  }
  ~GpsReceiver() = default;

  // callback from the uart rx
  void update(uint8_t data) override;

  void write(const char *data, uint8_t len) {
    dev->write((const uint8_t*)data, len);
  }

  UartSerial *dev;
  uint8_t *rxbuf;
  uint8_t writepos;
};

void GpsReceiver::update(uint8_t data) {
  if (bufferOpen) {
    rxbuf[writepos] = data;

    if (data == '\n') {
      bufferOpen = false;
      event_ISR_signal(sensor.info.event);
      writepos = 0;
    }
    else {
      writepos = (writepos+1) % 128;
    }
  }
}

static UartSerial::ptr createGpsUart() {
  static UartSerial gpsUart(GPS_USART_ID);
  return &gpsUart;
}

// turn on GPRMC and GGA
#define PMTK_SET_NMEA_OUTPUT_RMCGGA "$PMTK314,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0*28"

static void init(void) {
  static GpsReceiver gps(createGpsUart(), nmeainput);

  const char *enableRMCGGA = PMTK_SET_NMEA_OUTPUT_RMCGGA;
  gps.write(enableRMCGGA, strlen(enableRMCGGA));
}

static uint8_t poll(float *data, uint8_t size) {
  //  Poll the sensor for new data.  Copy the received sensor data into the provided data buffer.
  //  Return the number of floats copied.  If no data is available, return 0.
  uint8_t rv = 0;

  if (parse((char*)nmeainput)) {
    data[0] = latitudeDegrees;
    data[1] = longitudeDegrees;
    rv =  2;
  }

  bufferOpen = true;
  return rv;
}

SensorContext *gps_setup(uint16_t pollInterval, uint8_t aggregator_id) {
  //  Set up and return the sensor context.
  setup_sensor_context(&sensorContext, &sensor, pollInterval, aggregator_id);
  return &sensorContext;
}

uint8_t parseHex(char c) {
    if (c < '0')
      return 0;
    if (c <= '9')
      return c - '0';
    if (c < 'A')
       return 0;
    if (c <= 'F')
       return (c - 'A')+10;
    // if (c > 'F')
    return 0;
}

static bool parse(char *nmea) {
  if (strlen(nmea) < 4) return false;

  if (nmea[strlen(nmea)-4] == '*') {
    uint16_t sum = parseHex(nmea[strlen(nmea)-3]) * 16;
    sum += parseHex(nmea[strlen(nmea)-2]);

    // check checksum
    for (uint8_t i=2; i < (strlen(nmea)-4); i++) {
      sum ^= nmea[i];
    }
    if (sum != 0) {
      // bad checksum :(
      return false;
    }
  }


  int32_t degree;
  long minutes;
  char degreebuff[10];
  int32_t latitude_fixed;
  int32_t longitude_fixed;


  char lat;
  char lon;

  // look for a few common sentences
  if (strstr(nmea, "$GPGGA")) {
    // found GGA
    char *p = nmea;
    // skip the time info
    p = strchr(p, ',')+1;

    // parse out latitude
    p = strchr(p, ',')+1;
    if (',' != *p)
    {
      strncpy(degreebuff, p, 2);
      p += 2;
      degreebuff[2] = '\0';
      degree = atol(degreebuff) * 10000000;
      strncpy(degreebuff, p, 2); // minutes
      p += 3; // skip decimal point
      strncpy(degreebuff + 2, p, 4);
      degreebuff[6] = '\0';
      minutes = 50 * atol(degreebuff) / 3;
      latitude_fixed = degree + minutes;
      latitude = degree / 100000 + minutes * 0.000006F;
      latitudeDegrees = (latitude-100*int(latitude/100))/60.0;
      latitudeDegrees += int(latitude/100);
    }

    p = strchr(p, ',')+1;
    if (',' != *p)
    {
      if (p[0] == 'S') latitudeDegrees *= -1.0;
      if (p[0] == 'N') lat = 'N';
      else if (p[0] == 'S') lat = 'S';
      else if (p[0] == ',') lat = 0;
      else return false;
    }

    // parse out longitude
    p = strchr(p, ',')+1;
    if (',' != *p)
    {
      strncpy(degreebuff, p, 3);
      p += 3;
      degreebuff[3] = '\0';
      degree = atol(degreebuff) * 10000000;
      strncpy(degreebuff, p, 2); // minutes
      p += 3; // skip decimal point
      strncpy(degreebuff + 2, p, 4);
      degreebuff[6] = '\0';
      minutes = 50 * atol(degreebuff) / 3;
      longitude_fixed = degree + minutes;
      longitude = degree / 100000 + minutes * 0.000006F;
      longitudeDegrees = (longitude-100*int(longitude/100))/60.0;
      longitudeDegrees += int(longitude/100);
    }

    p = strchr(p, ',')+1;
    if (',' != *p)
    {
      if (p[0] == 'W') longitudeDegrees *= -1.0;
      if (p[0] == 'W') lon = 'W';
      else if (p[0] == 'E') lon = 'E';
      else if (p[0] == ',') lon = 0;
      else return false;
    }
    return true;
  }

  return false;
}

