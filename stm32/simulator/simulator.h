//  Simulator Module that captures, replays and simulates SPI commands for SPI sensors.
//  We do this so that we can capture the SPI send/receive commands of Arduino sensor drivers and replay
//  them efficiently on STM32, with multitasking.  And also for testing with simulated sensors.  Simulator supports 3 modes:
//  (1) Capture Mode: Intercept all SPI transceive commands. Capture all SPI transmit/receive packets into the Simulator Trail in memory.
//  (2) Replay Mode: Replay each SPI transmit packet captured earlier in the trail.  Record the SPI received packets into the Simulator Trail.
//  (3) Simulate Mode: Intercept all SPI transceive commands. Instead of sending actual SPI packets, the Simulator injects the received SPI packets
//      from Replay Mode and returns the injected packets as the result of the SPI transceive command.
//
//  We run the Simulator modes like this: Capture -> Replay -> Simulate -> Replay -> Simulate -> Replay -> Simulate ...
//  This is efficient for multitasking because in Replay Mode we can use a Completion Semaphore to wait for the SPI receive data.
//  We may also merge multiple transmit/receive packets and send them in one blast of SPI data (if the sensor allows).  Simulate Mode
//  requires little execution time because we are injecting data recorded from Replay Mode.
//
//  The SPI Control captures SPI transmit/receive packets into the trail like this:
//  [1 byte: tx_length] [tx_length bytes: tx_buffer] [1 byte: rx_length] [rx_length bytes: rx_buffer] ... (next packet)
#ifndef SIMULATOR_H_
#define SIMULATOR_H_
#include <stdint.h>  //  For uint8_t
#include <stdlib.h>  //  For size_t
#include <cocoos.h>  //  For Evt_t

#define MAX_SIM_NAME_SIZE 3  //  Max number of letters/digits in sensor name.  TODO: Sync with MAX_SENSOR_NAME_SIZE in sensor.h
#define MAX_TRAIL_SIZE 64    //  Max number of bytes per trail.
//#define MAX_MERGED_SIZE (MAX_TRAIL_SIZE >> 1)  //  Max number of bytes for storing merged trail.
#define MAX_MERGED_SIZE (MAX_TRAIL_SIZE)  //  Max number of bytes for storing merged trail.

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
	uint8_t id;	 // Sensor ID
    char    name[MAX_SIM_NAME_SIZE + 1]; //  Sensor name e.g. "tmp".
    uint8_t trail[MAX_TRAIL_SIZE];       //  Captured trail.  Note: MUST be a buffer in static memory, not on the stack.
    uint8_t merged_tx[MAX_MERGED_SIZE];  //  Merged transmit buffer.  Note: MUST be a buffer in static memory, not on the stack.
    uint8_t merged_rx[MAX_MERGED_SIZE];  //  Merged receive buffer.  Note: MUST be a buffer in static memory, not on the stack.
    uint8_t index;             //  Index of trail to be processed next.
	uint8_t	length;		       //  Trail length.
    uint8_t merged_length;     //  Length of merged trail.
    bool    capture_enabled;   //  True if simulator should capture SPI commands.
    bool    replay_enabled;    //  True if simulator should replay SPI commands.
    bool    simulate_enabled;  //  True if simulator should simulate SPI commands.
    bool    merge_enabled;     //  True if simulator should merge multiple SPI commands into one command.
    bool    replayed_merge;    //  True if we have just replayed the merged trail.
    Simulator_Mode mode;       //  Simulator mode.
    Sem_t semaphore;           //  Binary Semaphore that will be signalled by SPI Control when a replay ends.
    SPI_Control *port;         //  SPI port for the simulator.
	Simulator_Fails	failCode;  //  Last fail code.
};

#ifdef USE_SIMULATOR  // If Simulator is enabled...
Simulator_Fails simulator_setup(void);  //  Set up the simulator system.
Simulator_Fails simulator_configure(Simulator_Control *sim, uint8_t id, const char *name, SPI_Control *port,
    bool capture_enabled, bool replay_enabled, bool simulate_enabled, bool merge_enabled);  //  Set up the simulator for the sensor.
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

//  Append the send/receive packet to the merged trail.
Simulator_Fails simulator_merge_packet(Simulator_Control *sim, uint8_t *tx_packet, int tx_size, uint8_t *rx_packet, int rx_size);
bool simulator_should_replay_merged_trail(Simulator_Control *sim);  //  Return true if simulator should replay the merged trail instead of the unmerged trail.

Simulator_Fails simulator_close(Simulator_Control *sim);  //  End capture, replay or simulate.
#else  //  If Simulator is disabled...
#define simulator_setup() {}
#define simulator_configure(sim, id, name, port, capture_enabled, replay_enabled, simulate_enabled, merge_enabled) {}
#define simulator_open(sim) {}
#define simulator_capture_size(sim, size) 0
#define simulator_capture_packet(sim, packet, size) NULL
#define simulator_replay_size(sim) 0
#define simulator_replay_packet(sim, size) NULL
#define simulator_simulate_size(sim) 0
#define simulator_simulate_packet(sim, size) NULL
#define simulator_merge_packet(sim, tx_packet, tx_size, rx_packet, rx_size) {}
#define simulator_should_replay_merged_trail(sim) false
#define simulator_replay(sim) NULL
#define simulator_close(sim) {}
#define simulator_test(sim) {}
#define simulator_should_poll_sensor(sim) true  //  Always poll the sensor.

#endif  //  USE_SIMULATOR

#ifdef __cplusplus
}  //  End of extern C scope.
#endif

#endif  //  SIMULATOR_H_
