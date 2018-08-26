
#ifndef RADIODEVICE_H_
#define RADIODEVICE_H_

#include "uartSerial.h"
#include "wisol.h"

// Base class for radio chip devices
// Should be implemented by specialized classes for specific chips in the radios subfolder
class RadioDevice : public UartSerial::IReaderCb {
public:
  RadioDevice() = default;
  virtual ~RadioDevice() = default;
  virtual unsigned getStepBegin(NetworkCmd list[], int listSize, bool useEmulator) = 0;
  virtual bool send(const uint8_t *data, uint8_t len) = 0;
  virtual uint8_t receive(uint8_t *buf) = 0;
  virtual void update(uint8_t data) = 0;
  virtual void setDoneEvent(Evt_t event) = 0;
  virtual void setMarkerCount(unsigned count) = 0;
};

#endif /* RADIODEVICE_H_ */
