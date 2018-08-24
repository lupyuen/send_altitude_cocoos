#ifndef WISOL_CONTROLLER_H_
#define WISOL_CONTROLLER_H_

#include <cocoos.h>
#include "radioController.h"
#include "uartSerial.h"



// A class for communicating with the Wisol radio chip
class WisolController : public RadioController {
public:
  WisolController(UartSerial *serial);
  ~WisolController() = default;

  // send a data buffer to the radio
  bool send(const uint8_t *data, uint8_t len) override;

  // fetch message received by the radio.
  // should be called after the rxDoneEvent have been signaled
  uint8_t receive(uint8_t *buf) override;

  // callback from the uart rx
  void update(uint8_t data) override;

  // provide an event that should be signaled when a complete
  // message have been received by the radio
  void setDoneEvent(Evt_t event) override;

  void setMarkerCount(unsigned count) override;
private:
  UartSerial *dev;
  uint8_t rxbuf[128];
  uint8_t writepos;
  uint8_t readpos;
  Evt_t rxDoneEvt;
  unsigned expectedMarkerCount;
};

#endif /* WisolController */
