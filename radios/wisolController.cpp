#include "wisolController.h"
#include "cocoos.h"

WisolController::WisolController(UartSerial *serial):
  dev(serial),
  writepos(0),
  readpos(0),
  rxDoneEvt(NO_EVENT),
  expectedMarkerCount(0),
  receivedMarkers(0){
  dev->registerReader(this);
}

bool WisolController::send(const uint8_t *data, uint8_t len) {
  return dev->write(data, len);
}

uint8_t WisolController::receive(uint8_t *buf) {

    if (receivedMarkers == expectedMarkerCount) {
        uint8_t cnt = 0;
        uint8_t *data = &rxbuf[0];
        uint8_t nbytes = 0;

        while(cnt < expectedMarkerCount) {
            *buf++ = *data;
            nbytes++;
            if (*data == '\n') {
                cnt++;
            }
            data++;
        }
        receivedMarkers = 0;
        return nbytes;
    }
    return 0;
}

void WisolController::setDoneEvent(Evt_t event) {
  rxDoneEvt = event;
}

void WisolController::setMarkerCount(unsigned count) {
    expectedMarkerCount = count;
}

void WisolController::update(uint8_t data) {
  rxbuf[writepos] = data;

  if ((data == '\n') && (++receivedMarkers == expectedMarkerCount)){

    // we have received a full message, signal upper layer
    if (NO_EVENT != rxDoneEvt) {
      event_ISR_signal(rxDoneEvt);
    }
    // We just hope that the message is read before new data arrives
    // TODO: put received data in a thread safe circular buffer
    writepos = 0;
  }
  else {
    writepos = (writepos+1) % 128;
  }
}




