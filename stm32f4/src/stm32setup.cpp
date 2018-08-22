/*
 * stm32setup.cpp
 *
 */

#include "stm32setup.h"
#include <inc/stm32f4xx.h>
#include "cocoos.h"
#include "pin.h"

static void initRCC();

void stm32_setup() {
  /* Enable the peripheral clocks */
  initRCC();

  /* Set SysTick according to 1 ms tick */
  /* SystemCoreClock = 168000000 */
  if ( SysTick_Config( (SystemCoreClock / 1000 ) * 1 ) ) {
      /* Capture error */
      while (1);
  }

  NVIC_SetPriority (SysTick_IRQn, 0);
  NVIC_PriorityGroupConfig(NVIC_PriorityGroup_2);
  pin::init();

}

extern "C"
{
/**
  * @brief  This function handles SysTick Handler.
  * @param  None
  * @retval None
  */
void SysTick_Handler(void)
{
    os_tick();
}


}

static void initRCC() {

    RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA |
                          RCC_AHB1Periph_GPIOB |
                          RCC_AHB1Periph_GPIOC |
                          RCC_AHB1Periph_GPIOD |
                          RCC_AHB1Periph_GPIOE |
                          RCC_AHB1Periph_GPIOF, ENABLE);

    RCC_AHB2PeriphClockCmd(RCC_AHB2Periph_OTG_FS, ENABLE) ;

    RCC_APB2PeriphClockCmd(RCC_APB2Periph_USART1 |
                            RCC_APB2Periph_USART6 |
                            RCC_APB2Periph_SYSCFG, ENABLE);

    RCC_APB1PeriphClockCmd(RCC_APB1Periph_USART2 |
                           RCC_APB1Periph_USART3 |
                           RCC_APB1Periph_UART4  |
                           RCC_APB1Periph_UART5 |
                           RCC_APB1Periph_CAN1, ENABLE);
}


