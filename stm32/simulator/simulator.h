//  Simulator Module that captures, replays and simulates SPI commands for SPI sensors.
//  We do this so that we can capture the SPI send/receive commands of Arduino sensor drivers and replay
//  them efficiently on STM32, with multitasking.  And also for testing with simulated sensors.
#ifndef SIMULATOR_H_
#define SIMULATOR_H_
#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t
#include <cocoos.h>  //  For Evt_t

#define MAX_SENSOR_COUNT 3  //  Max number of sensors supported.  TODO: Sync with platform.h
#define MAX_SENSOR_NAME_SIZE 3  //  Max number of letters/digits in sensor name.  TODO: Sync with sensor.h
#define MAX_TRAIL_SIZE 64  //  Max number of bytes per trail.

#ifdef __cplusplus
extern "C" {  //  Allows functions below to be called by C and C++ code.
#endif

enum Simulator_Fails {  //  Error codes.
    Simulator_Ok = 0,
    Simulator_Invalid_Size,
    Simulator_Trail_Overflow,
    Simulator_Missing_Port,
    Simulator_Write_Timeout,
    Simulator_Read_Timeout,
    Simulator_End,  //  Insert new codes above.
};

enum Simulator_Mode {  //  Simulator modes.
    Simulator_Capture = 0,
    Simulator_Replay = 1,
    Simulator_Simulate = 2,
    Simulator_Mismatch = 3,  //  Simulation failed due to mismatch.
    Simulator_Disabled = 4,
};

struct SPI_Control;

struct Simulator_Control {
	uint32_t	id;	 // Sensor ID
    char        name[MAX_SENSOR_NAME_SIZE + 1];  //  Sensor name e.g. "tmp".
    uint8_t     trail[MAX_TRAIL_SIZE];  //  Captured trail.  Note: MUST be a buffer in static memory, not on the stack.
	uint8_t		length;		       //  Trail length.
    uint8_t     index;             //  Index of trail to be processed next.
    bool        capture_enabled;   //  True if simulator should capture SPI commands.
    bool        replay_enabled;    //  True if simulator should replay SPI commands.
    bool        simulate_enabled;  //  True if simulator should simulate SPI commands.
    Simulator_Mode mode;           //  Simulator mode.
    Sem_t semaphore;
    SPI_Control *port;             //  SPI port for the simulator.
	Simulator_Fails	failCode;      //  Last fail code.
};

Simulator_Fails simulator_setup(void);  //  Set up the simulator system.
Simulator_Fails simulator_configure(Simulator_Control *sim, uint32_t id, const char *name, SPI_Control *port,
    bool capture_enabled, bool replay_enabled, bool simulate_enabled);  //  Set up the simulator for the sensor.
Simulator_Fails simulator_open(Simulator_Control *sim);  //  Begin capture, replay or simulate.
bool simulator_should_poll_sensor(Simulator_Control *sim);  //  Return true if the Sensor Task should actually poll the sensor.
Sem_t *simulator_replay(Simulator_Control *sim);  //  Replay the captured SPI commands.
bool simulator_is_request_complete(Simulator_Control *sim);  //  Return true if last SPI command was completed successfully or with error.
bool simulator_is_request_successful(Simulator_Control *sim);  //  Return true if last SPI command was successful.
Simulator_Fails simulator_dump_packet(Simulator_Control *sim);  //  Dump the last SPI packet to console.

//  Capture, replay or simulate an SPI send/receive packet, which has a packet size.
//  One Trail = One or more SPI Commands per sensor.
//  One SPI Command = One Send Packet + One Receive Packet
//  Receive Packet content will be overwritten during replay.

//  Append the packet size and packet content to the trail.
Simulator_Fails   simulator_capture_size(Simulator_Control *sim, int size);
Simulator_Fails   simulator_capture_packet(Simulator_Control *sim, uint8_t *packet, int size);
//  For replay, fetch the next packet size and next packet content from the trail.  Packet content will be overwritten for receive packets.
int      simulator_replay_size(Simulator_Control *sim);
uint8_t *simulator_replay_packet(Simulator_Control *sim, int size);
//  For simulation, fetch the next packet size and next packet content from the trail.
int      simulator_simulate_size(Simulator_Control *sim);
uint8_t *simulator_simulate_packet(Simulator_Control *sim, int size);

Simulator_Fails simulator_close(Simulator_Control *sim);  //  End capture, replay or simulate.

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#endif  //  SIMULATOR_H_
