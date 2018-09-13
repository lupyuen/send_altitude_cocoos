//  Simulator Module that captures, replays and simulates SPI commands for SPI sensors.
#include <string.h>
#include <logger.h>
#include <bluepill.h>  //  For millis()
#include <spiint.h>    //  For SPI_Control
#include "simulator.h"

//  TODO: Update this when running under FreeRTOS.
#define systicks millis
typedef uint32_t TickType_t;

//  Messages for each fail code.
static const char *simulator_msg[] = {
	"OK",
	"Invalid size",  //  Simulator_Invalid_Size
	"Trail overflow",  //  Simulator_Trail_Overflow
    "Missing port",  //  Simulator_Missing_Port
	"Write timeout",
	"Read timeout"
};

static inline TickType_t diff_ticks(TickType_t early,TickType_t later) {
    //  Compute the difference in ticks.
	if ( later >= early )
		return later - early;
	return ~(TickType_t)0 - early + 1 + later;
}

const char *simulator_error(Simulator_Fails fcode) {
    //  Return a character string message for Simulator_Fails code
	int icode = (int)fcode;
	if ( icode < 0 || icode >= (int)Simulator_End )
		return "Bad Simulator_Fails code";
	return simulator_msg[icode];
}

static Simulator_Fails showError(Simulator_Control *sim, Simulator_Fails fc) {
	if (sim) { sim->failCode = fc; }
	debug_print("***** Error: Simulator Failed ");
	debug_print(fc); debug_print(" / ");
	debug_println(simulator_error(fc));
	debug_flush();
	return fc;
}

Simulator_Fails simulator_setup(void) { 
    //  Set up the simulator system.
    return Simulator_Ok;
}

Simulator_Fails simulator_configure(
    Simulator_Control *sim, 
    uint32_t id, 
    const char *name, 
    volatile SPI_Control *port) {
    //  Set up the simulator for the sensor.
    sim->mode = Simulator_Capture;  //  Always capture the first time.
    sim->index = 0;
    sim->length = 0;
    sim->id = id;
    sim->port = port;
    if (port) { port->simulator = sim; }
    if (name) {
        strncpy(sim->name, name, MAX_SENSOR_NAME_SIZE);
        sim->name[MAX_SENSOR_NAME_SIZE] = 0;
    }
    return Simulator_Ok;
}

Simulator_Fails simulator_open(Simulator_Control *sim) {
    //  Begin capture, replay or simulate.  Set the simulator in the port.
    debug_println("sim open");
    volatile SPI_Control *port = sim->port;
    if (port == NULL) { return showError(sim, Simulator_Missing_Port); }
    sim->index = 0;
    //  Simulator depends on sensor ID, so we need to refresh the port.
    port->simulator = sim;

    //  For Replay Mode, open the SPI port.
    if (sim->mode == Simulator_Replay) {
        spi_open(port);
    }
    return Simulator_Ok;
}

bool simulator_should_poll_sensor(Simulator_Control *sim) {
    //  Return true if the Sensor Task should actually poll the sensor, i.e. when capturing or when simulating.
    if (sim->mode == Simulator_Replay) { return false; }
    return true;
}

volatile Evt_t *simulator_replay(Simulator_Control *sim) {
    //  Replay the captured SPI commands.  Return an event that the Sensor Task should wait for completion.
    //  Return NULL if no more packets to replay.
    if (sim->mode != Simulator_Replay || sim->port == NULL) { return NULL; }
    return spi_transceive_replay(sim->port);
}

static Simulator_Fails simulator_overflow(Simulator_Control *sim) {
    //  Handle an overflow.  Switch to error mode.
    debug_println("sim overflow");
    sim->mode = Simulator_Mismatch;
    return showError(sim, Simulator_Trail_Overflow);
}

//  Capture, replay or simulate an SPI send/receive packet, which has a packet size.
//  One Trail = One or more SPI Commands per sensor.
//  One SPI Command = One Send Packet + One Receive Packet
//  Receive Packet content will be overwritten during replay.

Simulator_Fails simulator_capture_size(Simulator_Control *sim, int size) {
    //  Append the packet size to the trail.  Don't append if there was an error.
    if (size <= 0) { return showError(sim, Simulator_Invalid_Size); }
    if ((sim->index + 1) >= MAX_TRAIL_SIZE)  { return simulator_overflow(sim); }
    if (sim->mode == Simulator_Mismatch) { return Simulator_Trail_Overflow; }

    sim->trail[sim->index] = (uint8_t) size;
    sim->index++;
    sim->length = sim->index;
    return Simulator_Ok;
}

Simulator_Fails simulator_capture_packet(Simulator_Control *sim, volatile uint8_t *packet, int size) {
    //  Append the packet content to the trail.  Don't append if there was an error.
    if (size <= 0 || packet == NULL) { return showError(sim, Simulator_Invalid_Size); }
    if ((sim->index + size) >= MAX_TRAIL_SIZE)  { return simulator_overflow(sim); }
    if (sim->mode == Simulator_Mismatch) { return Simulator_Trail_Overflow; }

    memcpy((void *) &(sim->trail[sim->index]), (const void *) packet, size);  //  Copy the packet.
    sim->index = sim->index + size;
    sim->length = sim->index;
    return Simulator_Ok;
}

//  For replay, fetch the next packet size and next packet content from the trail.  Packet content will be overwritten for receive packets.
int simulator_replay_size(Simulator_Control *sim) {
    //  Return -1 in case of error.
    if ((sim->index + 1) >= MAX_TRAIL_SIZE || (sim->index + 1) >= sim->length)  { return -1; }
    uint8_t size = sim->trail[sim->index];
    sim->index++;
    return size;
}

volatile uint8_t *simulator_replay_packet(Simulator_Control *sim, int size) {
    //  Return NULL in case of error.
    if (size <= 0) { return NULL; }
    if ((sim->index + size) >= MAX_TRAIL_SIZE || (sim->index + size) >= sim->length)  { return NULL; }
    volatile uint8_t *packet = &(sim->trail[sim->index]);
    sim->index = sim->index + size;
    return packet;
}

//  For simulation, fetch the next packet size and next packet content from the trail.
int simulator_simulate_size(Simulator_Control *sim) {
    return simulator_replay_size(sim);
}

volatile uint8_t *simulator_simulate_packet(Simulator_Control *sim, int size) {
    return simulator_replay_packet(sim, size);
}

bool simulator_is_request_completed(Simulator_Control *sim) {
    //  Return true if last SPI command was completed.
    if (!sim->port) return true;
    return spi_is_transceive_completed(sim->port);
}

Simulator_Fails simulator_dump_packet(Simulator_Control *sim) {
    //  Dump the last SPI packet to console.
    if (!sim->port) return Simulator_Ok;
    spi_dump_packet(sim->port);
    return Simulator_Ok;
}

Simulator_Fails simulator_close(Simulator_Control *sim) {  
    //  End capture, replay or simulate.  Remove the simulator from the port.
    //  Set the next mode: Capture -> Replay -> Simulate.
    debug_println("sim close");
    //  For Replay Mode, close the SPI port.
    if (sim->port != NULL && sim->mode == Simulator_Replay) {
        spi_dump_packet(sim->port);
        spi_close(sim->port);
    }
    if (sim->port != NULL) { spi_dump_trail(sim->port); }  //  Dump the trail for debug.
    switch (sim->mode) {
        case Simulator_Capture:  //  After capture, replay.
            if (sim->length > 0) { sim->mode = Simulator_Replay; }
            else { sim->mode = Simulator_Mismatch; }  //  Nothing recorded, don't replay.
            break;
        case Simulator_Replay: sim->mode = Simulator_Simulate; break;  //  After replay, simulate.
        case Simulator_Simulate: sim->mode = Simulator_Replay; break;  //  After simulate, replay.
        case Simulator_Mismatch:   //  If replay failed, capture again.
            sim->length = 0;
            sim->mode = Simulator_Capture; 
            break;
        default: debug_print("***** ERROR: Unknown simulator mode "); debug_println(sim->mode); debug_flush();
    }
    if (sim->port != NULL) { sim->port->simulator = NULL; }
    return Simulator_Ok;
}

Simulator_Fails simulator_test(Simulator_Control *sim) {  
    //  For testing only.
    return Simulator_Ok;
}
