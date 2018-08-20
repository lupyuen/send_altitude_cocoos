/*
 * apptypes.h
 *
 *  Created on: 2 apr 2016
 *      Author: peter_000
 */

#ifndef APPTYPES_H_
#define APPTYPES_H_

#include <inc/queue.h>
#include <inc/utilstypes.h>



#define PRINT_MSG			0x40
#define PRINT_MULTI_MSG		0x41
#define PRINT_NEW_LINE		0x42

#define ADD_DRIVER			0x50
#define REMOVE_DRIVER	    0x51

#define LIST 				0x60
#define CD 					0x61

namespace usbapp {

typedef utils::Queue<utils::string> StrQueue;

}


#endif /* APPTYPES_H_ */
