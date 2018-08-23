#include <stm32f4uart.h>
#include <config.h>
#include <inc/stm32f4xx.h>
#include <string.h>



static usart::ptr usart_inst[ usart::N_MAX_USARTS ] = {0};
static usart::ptr irq_inst[ usart::N_MAX_USARTS ] = {0};

usart::usart( ID id ):
    reader(nullptr),
    m_id(id),
    n_write(0),
    next(nullptr),
    busy(false) {
    usart_inst[ id ] = this;
    irq_inst[ irqnum(id) ] = this;
    txdata = settings[id]->out;
    init();
}


usart::~usart() {
    usart_inst[ m_id ] = 0;
}

usart::ptr usart::instance( ID id ) {

    if (( id >= usart::N_MAX_USARTS ) || ( id > n_configured_usarts - 1 )) return 0;
    return usart_inst[ id ];
}

uint8_t usart::irqnum( ID id ) {
    uint8_t irqn = settings[id]->nvic.NVIC_IRQChannel;
    switch (irqn) {
        case USART1_IRQn: return 0;
        case USART2_IRQn: return 1;
        case USART3_IRQn: return 2;
        case UART4_IRQn: return 3;
        case UART5_IRQn: return 4;
        case USART6_IRQn: return 5;
    }
    return 0;
}

void usart::init() {
    USART_Init(settings[m_id]->usart, (USART_InitTypeDef*)&(settings[m_id]->init));
    NVIC_Init((NVIC_InitTypeDef*)&(settings[m_id]->nvic));
    //USART_OverrunDetectionConfig(settings[m_id]->usart, USART_OVRDetection_Enable);

    // Clear RXNE flag by reading the rx register
    USART_ReceiveData(settings[m_id]->usart);

    // clear pending interrupts
    USART_ClearITPendingBit(settings[m_id]->usart,USART_IT_ORE);
    USART_ClearITPendingBit(settings[m_id]->usart,USART_IT_NE);
    USART_ClearITPendingBit(settings[m_id]->usart,USART_IT_FE);
    USART_ClearITPendingBit(settings[m_id]->usart,USART_IT_PE);

    // enable interrupts
    USART_ITConfig(settings[m_id]->usart, USART_IT_RXNE, ENABLE);
    USART_ITConfig(settings[m_id]->usart, USART_IT_PE, ENABLE);
    USART_ITConfig(settings[m_id]->usart, USART_IT_ERR, ENABLE);

    USART_ClockInitTypeDef USART_ClockInitStruct;
    USART_ClockStructInit(&USART_ClockInitStruct);
    USART_ClockInit(settings[m_id]->usart, &USART_ClockInitStruct);

    USART_Cmd(settings[m_id]->usart, ENABLE);
}

void usart::registerReader(IReaderCb* rd) {
    reader = rd;
}

void usart::isr() {
  // tx?
  if ( SET == USART_GetITStatus(settings[m_id]->usart,USART_IT_TXE) ) {
    if (n_write--) {
      USART_SendData(settings[m_id]->usart, *next++);
    }
    else {
      // transmission complete - disable TXE interrupt
      USART_ClearITPendingBit(settings[m_id]->usart, USART_IT_TXE);
      USART_ITConfig(settings[m_id]->usart, USART_IT_TXE, DISABLE);
      busy = false;
    }
  }
  else if ( SET == USART_GetITStatus(settings[m_id]->usart,USART_IT_RXNE) ) {
    uint16_t data = USART_ReceiveData(settings[m_id]->usart);
    if ( reader ) {
      reader->usart_update(data);
    }
    else {
    }
  }
}

bool usart::write(uint8_t *buf, uint8_t len) {
  if (not busy && len <= settings[m_id]->bufsz) {
    // Copy data buffer to output buffer and start transmission
    // by enabling TXE interrupt
    n_write = len;

    memcpy(txdata, buf, len);

    next = &txdata[0];
    busy = true;
    USART_ClearITPendingBit(settings[m_id]->usart, USART_IT_TXE);
    USART_ITConfig(settings[m_id]->usart, USART_IT_TXE, ENABLE);
    return true;
  }

  return false;
}


extern "C" {

void USART1_IRQHandler() {
  irq_inst[ 0 ]->isr();
}


void USART2_IRQHandler() {
  irq_inst[ 1 ]->isr();
}


void USART3_IRQHandler() {
  irq_inst[ 2 ]->isr();
}


void UART4_IRQHandler() {
  irq_inst[ 3 ]->isr();
}


void UART5_IRQHandler() {
  irq_inst[ 4 ]->isr();
}

void USART6_IRQHandler() {
  irq_inst[ 5 ]->isr();
}

}
