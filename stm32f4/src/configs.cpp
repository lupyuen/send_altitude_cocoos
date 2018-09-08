
#include <config.h>

#define DEBUG_BUF_SZ    128


// buffer for outgoing debug usart data
// data will be copied into this buffer when performing a uart::write() call
static uint8_t debug[DEBUG_BUF_SZ];

static uint8_t gpsrx[DEBUG_BUF_SZ];

UartSerial::cfg_t wisolUartCfg =    {   USART2,
                                {
                                    9600,
                                    USART_WordLength_8b,
                                    USART_StopBits_1,
                                    USART_Parity_No,
                                    USART_Mode_Rx|USART_Mode_Tx,
                                    USART_HardwareFlowControl_None
                                },
                                {
                                    USART2_IRQn,
                                    0,
                                    10,
                                    ENABLE
                                },
                                debug,
                                DEBUG_BUF_SZ
                            };

UartSerial::cfg_t gpsUartCfg =    {   USART1,
                                      {
                                          9600,
                                          USART_WordLength_8b,
                                          USART_StopBits_1,
                                          USART_Parity_No,
                                          USART_Mode_Rx|USART_Mode_Tx,
                                          USART_HardwareFlowControl_None
                                      },
                                      {
                                          USART1_IRQn,
                                          0,
                                          9,
                                          ENABLE
                                      },
                                      gpsrx,
                                      DEBUG_BUF_SZ
                                  };

const UartSerial::cfg_t *UartSerial::settings[] = { &wisolUartCfg, &gpsUartCfg };
const uint16_t UartSerial::n_configured_usarts = 2;


pin::cfg_t pin0 = {GPIOD, {GPIO_Pin_13, GPIO_Mode_OUT, GPIO_High_Speed, GPIO_OType_PP, GPIO_PuPd_DOWN}, {0, 0}};
pin::cfg_t pin1 = {GPIOD, {GPIO_Pin_12, GPIO_Mode_OUT, GPIO_High_Speed, GPIO_OType_PP, GPIO_PuPd_DOWN}, {0, 0}};

pin::cfg_t pin2 = {GPIOB, {GPIO_Pin_6, GPIO_Mode_AF, GPIO_High_Speed, GPIO_OType_PP, GPIO_PuPd_NOPULL}, {GPIO_PinSource6, GPIO_AF_USART1}};
pin::cfg_t pin3 = {GPIOB, {GPIO_Pin_7, GPIO_Mode_AF, GPIO_High_Speed, GPIO_OType_PP, GPIO_PuPd_NOPULL}, {GPIO_PinSource7, GPIO_AF_USART1}};

pin::cfg_t pin7 = {GPIOA, {GPIO_Pin_2, GPIO_Mode_AF, GPIO_High_Speed, GPIO_OType_PP, GPIO_PuPd_NOPULL}, {GPIO_PinSource2, GPIO_AF_USART2}};
pin::cfg_t pin8 = {GPIOA, {GPIO_Pin_3, GPIO_Mode_AF, GPIO_High_Speed, GPIO_OType_PP, GPIO_PuPd_NOPULL}, {GPIO_PinSource3, GPIO_AF_USART2}};


const uint16_t pin::n_configured_pins = 6;
pin::cfg_t *pin::pinConfigs[] = {&pin0, &pin1, &pin2, &pin3, &pin7, &pin8 };




