/*
 * utils.cpp
 *
 *  Created on: Sep 1, 2018
 *      Author: peter
 */

#include "utils.h"
#include <stddef.h>


void uint32ToStringBase16(char buffer[], uint32_t value)
{
    size_t length = 0u;
    uint32_t tmp = value;

    /* Calculate length of resulting string */
    while (tmp > 0u)
    {
        tmp /= 16u;
        length++;
    }

    if (length == 0u)
    {
        buffer[0] = '0';
        buffer[1] = (char)0;
    }
    else
    {
        // always set the length to 8 bytes
        length = 8;
        buffer[length] = (char)0;

        while (length > 0u)
        {
            uint32_t rest = value % 16u;

            if (rest < 10u)
            {
                uint32_t TempVal = 0x30u + rest;        /* Needed to pass MISRA */
                buffer[length - 1u] = (char)TempVal;
            }
            else
            {
                uint32_t TempVal = 0x41u + rest - 10u;
                buffer[length - 1u] = (char)(TempVal);
            }

            value /= 16u;
            length--;
        }
    }
}
