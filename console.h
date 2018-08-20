/*
 * console.h
 *
 *  Created on: 7 feb 2016
 *      Author: peter_000
 */

#ifndef CONSOLE_H_
#define CONSOLE_H_

#include <stdint.h>
#include <cocoos.h>
#include <inc/usart.h>
#include <command.h>
#include <apptypes.h>




typedef struct {
    Msg_t super;
    utils::string str;
} StringMsg_t;

class Console : public ::mcal::usart::reader {

public:
	Console(uint8_t read_prio, uint8_t write_prio);
	virtual ~Console();

	void create();
	static void writetask();
	static usbapp::StrQueue& queue();
	virtual void usart_update(::mcal::usart& u, uint8_t c);
	static uint8_t writetaskId();

private:
	static void readtask();
	uint8_t _rprio;
	uint8_t _wprio;
	StringMsg_t msgpool[32];
	static usbapp::StrQueue input;
	static usbapp::StrQueue output;
	utils::string message;
	static uint8_t writetask_id;
};



#endif /* CONSOLE_H_ */
