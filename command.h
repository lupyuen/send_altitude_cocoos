/*
 * command.h
 *
 *  Created on: 7 feb 2016
 *      Author: peter_000
 */

#ifndef COMMAND_H_
#define COMMAND_H_


#include <apptypes.h>
#include <string>
#include <inc/node.h>

#include <commander.h>

class DefaultCommand : public Command {
public:
	DefaultCommand() : Command("nocommand"){}
	virtual ~DefaultCommand() {}
	virtual void execute() {}
};



#endif /* COMMAND_H_ */
