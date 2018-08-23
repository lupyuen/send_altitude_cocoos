/*
 * radioController.h
 *
 *  Created on: Aug 23, 2018
 *      Author: peter
 */

#ifndef RADIOCONTROLLER_H_
#define RADIOCONTROLLER_H_

#include "uartSerial.h"

// Base class for radio chip controllers
// Should be implemented by specialized classes for specific chips in the radios subfolder
class RadioController : public UartSerial::IReaderCb {
public:
  RadioController() = default;
  virtual ~RadioController() = default;
  virtual bool send(uint8_t *data, uint8_t len) = 0;
  virtual void receive(uint8_t *buf) = 0;
  virtual void update(uint8_t data) = 0;
  virtual void setDoneEvent(Evt_t event) = 0;
};

#endif /* RADIOCONTROLLER_H_ */
