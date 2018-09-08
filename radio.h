
#ifndef RADIO_H_
#define RADIO_H_

#include "uartSerial.h"
#include "network.h"


// Base class for radio chip devices
// Should be implemented by specialized classes for specific chips in the radios subfolder
class Radio : public UartSerial::IReaderCb {
public:
  Radio() = default;
  virtual ~Radio() = default;
  virtual unsigned getStepBegin(struct NetworkCmd list[], int listSize, bool useEmulator) = 0;
  virtual unsigned getStepSend(struct NetworkContext *context, NetworkCmd list[], int listSize, const float payload[], bool enableDownlink) = 0;
  virtual bool send(const uint8_t *data, uint8_t len) = 0;
  virtual uint8_t receive(uint8_t *buf) = 0;
  virtual void update(uint8_t data) = 0;
  virtual void setDoneEvent(Evt_t event) = 0;
  virtual void setMarkerCount(unsigned count) = 0;
};

#endif /* RADIO_H_ */
