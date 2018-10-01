//  Simulator Module that captures, replays and simulates SPI commands for SPI sensors.
#include "../../platform.h"  //  For USE_SIMULATOR, 
#ifdef DISABLE_SIMULATOR_LOG
#define DISABLE_DEBUG_LOG  //  Disable debug logging for the Simulator.
#endif  //  DISABLE_SIMULATOR_LOG
#include <string.h>
#include <logger.h>
#include <bluepill.h>  //  For millis()
#include <spiint.h>    //  For SPI_Control
#include "simulator.h"

#ifdef USE_SIMULATOR  //  This code is included only when using simulator.

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

const char *simulator_error(Simulator_Fails fcode) {
    //  Return a character string message for Simulator_Fails code
	int icode = (int)fcode;
	if ( icode < 0 || icode >= (int)Simulator_End )
		return "Bad Simulator_Fails code";
	return simulator_msg[icode];
}

static Simulator_Fails showError(Simulator_Control *sim, const char *title, Simulator_Fails fc) {
	if (sim) { sim->failCode = fc; }
	debug_print("***** Error: Simulator Failed ");
	debug_print(title); debug_print(" ");
	debug_println(simulator_error(fc));
	debug_flush();
	return fc;
}

Simulator_Fails simulator_setup(void) { 
    //  Set up the simulator system.
    return Simulator_Ok;
}

Simulator_Fails simulator_configure(
    Simulator_Control *sim,   //  Simulator to be configured.
    uint8_t id,               //  Sensor ID.
    const char *name,         //  Sensor name e.g. "tmp".
    SPI_Control *port,        //  SPI port to intercept.
    bool capture_enabled,     //  True if simulator should capture SPI packets.
    bool replay_enabled,      //  True if simulator should replay SPI packets that were captured.
    bool simulate_enabled,    //  True if simulator should simulate SPI packets into the sensor code.
    bool merge_enabled) {     //  True if simulator should merge multiple SPI commands into one command.
    //  Set up the simulator for the sensor.
    sim->index = 0;
    sim->length = 0;
    sim->merged_length = 0;
    sim->id = id;
    sim->port = port;
    sim->capture_enabled = capture_enabled;
    sim->replay_enabled = replay_enabled;
    sim->simulate_enabled = simulate_enabled;
    sim->merge_enabled = merge_enabled;
    sim->replayed_merge = false;
    sim->semaphore = sem_bin_create(0);  //  Binary Semaphore: Will wait until signalled.
    if (name) {
        strncpy(sim->name, name, MAX_SIM_NAME_SIZE);
        sim->name[MAX_SIM_NAME_SIZE] = 0;
    }
    if (capture_enabled) {
        sim->mode = Simulator_Capture;  //  Always capture the first time.
        if (port) { port->simulator = sim; }
    } else {
        sim->mode = Simulator_Disabled;  //  Else disabled.
        if (port) { port->simulator = NULL; }
    }
    return Simulator_Ok;
}

Simulator_Fails simulator_open(Simulator_Control *sim) {
    //  Begin capture, replay or simulate.  Set the simulator in the SPI port.
    if (sim->mode == Simulator_Disabled) { return Simulator_Ok; }  //  If simulator disabled, quit.
    
    SPI_Control *port = sim->port;
    if (port == NULL) { return showError(sim, "simulator_open", Simulator_Missing_Port); }
    if (port) { debug_print("sim >> open spi"); debug_print((int) port->id); debug_print(" mode"); debug_println(sim->mode); }
    sim->index = 0;
    sim->replayed_merge = false;
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
    if (sim->mode == Simulator_Disabled) { return true; }  //  If simulator disabled, always poll the sensor.
    else if (sim->mode == Simulator_Replay) { return false; }
    return true;
}

Sem_t *simulator_replay(Simulator_Control *sim) {
    //  Replay the captured SPI commands.  Return a semaphore that the Sensor Task should wait for completion.
    //  Return NULL if no more packets to replay.
    if (sim->mode != Simulator_Replay || sim->port == NULL) { return NULL; }
    //  If we have just replayed a packet or merged trail, dump it.
    if (sim->index > 0 || sim->replayed_merge) { spi_dump_packet(sim->port); }

    //  For merged trail: Only 1 packet to send and receive.
    if (simulator_should_replay_merged_trail(sim)) {
        if (sim->replayed_merge) { return NULL; }  //  If already replayed the merged trail, stop.
        sim->replayed_merge = true; 
    } else if (sim->index >= sim->length) { return NULL; }  //  If no more packets, stop.

    //  Replay the next packet in the trail or the entire merged trail.
    SPI_Fails result = spi_transceive_replay(sim->port, &sim->semaphore);
    if (result != SPI_Ok) { return NULL; }
    //  Caller (Sensor Task) should wait for this completion semaphore before replaying next packet.
    return &sim->semaphore;
}

static Simulator_Fails simulator_overflow(Simulator_Control *sim) {
    //  Handle an overflow, i.e. no enough space to record new SPI commands.  
    //  Should increase MAX_TRAIL_SIZE and MAX_MERGE_SIZE.  Switch to error mode.
    debug_println("sim overflow"); debug_flush();
    sim->mode = Simulator_Mismatch;
    return showError(sim, "simulator_capture", Simulator_Trail_Overflow);
}

//  Capture, replay or simulate an SPI send/receive packet, which has a packet size.
//  One Trail = One or more SPI Commands per sensor.
//  One SPI Command = One Send Packet + One Receive Packet
//  Receive Packet content will be overwritten during replay.

Simulator_Fails simulator_capture_size(Simulator_Control *sim, int size) {
    //  Append the packet size to the trail.  Don't append if there was an error.
    if (size <= 0) { return showError(sim, "simulator_capture_size", Simulator_Invalid_Size); }
    if ((sim->index + 1) >= (MAX_TRAIL_SIZE + 1))  { return simulator_overflow(sim); }
    if (sim->mode == Simulator_Mismatch) { return Simulator_Trail_Overflow; }

    sim->trail[sim->index] = (uint8_t) size;
    sim->index++;
    sim->length = sim->index;
    return Simulator_Ok;
}

Simulator_Fails simulator_capture_packet(Simulator_Control *sim, uint8_t *packet, int size) {
    //  Append the packet content to the trail.  Don't append if there was an error.
    if (size <= 0 || packet == NULL) { return showError(sim, "simulator_capture_packet", Simulator_Invalid_Size); }
    if ((sim->index + size) >= (MAX_TRAIL_SIZE + 1))  { return simulator_overflow(sim); }
    if (sim->mode == Simulator_Mismatch) { return Simulator_Trail_Overflow; }

    memcpy((void *) &(sim->trail[sim->index]), (const void *) packet, size);  //  Copy the packet.
    sim->index = sim->index + size;
    sim->length = sim->index;
    return Simulator_Ok;
}

Simulator_Fails simulator_merge_packet(Simulator_Control *sim, uint8_t *tx_packet, int tx_size, uint8_t *rx_packet, int rx_size) {
    //  Append the send/receive packet to the merged trail.
    //  debug_print("sim merge tx "); debug_print(tx_size); debug_print(" rx "); debug_println(rx_size); debug_flush();
    if (tx_size <= 0 || tx_packet == NULL) { return showError(sim, "simulator_merge_packet", Simulator_Invalid_Size); }
    if (rx_size <= 0 || rx_packet == NULL) { return showError(sim, "simulator_merge_packet", Simulator_Invalid_Size); }
    if (tx_size != rx_size) { return showError(sim, "simulator_merge_packet", Simulator_Invalid_Size); }
    if ((sim->merged_length + tx_size) >= (MAX_MERGED_SIZE + 1))  { return simulator_overflow(sim); }
    if (sim->mode == Simulator_Mismatch) { return Simulator_Trail_Overflow; }

    memcpy((void *) &(sim->merged_tx[sim->merged_length]), (const void *) tx_packet, tx_size);  //  Copy the tx packet.
    memcpy((void *) &(sim->merged_rx[sim->merged_length]), (const void *) rx_packet, rx_size);  //  Copy the rx packet.
    sim->merged_length = sim->merged_length + tx_size;
    return Simulator_Ok;
}

bool simulator_should_replay_merged_trail(Simulator_Control *sim) {
    //  Return true if simulator should replay the merged trail instead of the unmerged trail.
    //  Check whether merge is enabled and merged trail is non-empty.
    if (sim->merge_enabled && sim->merged_length > 0) { return true; }
    return false;
}

//  For replay, fetch the next packet size and next packet content from the trail.  Packet content will be overwritten for receive packets.
int simulator_replay_size(Simulator_Control *sim) {
    //  Return -1 in case of error.
    if ((sim->index + 1) >= (MAX_TRAIL_SIZE + 1) 
        || (sim->index + 1) >= (sim->length + 1))  { return -1; }
    uint8_t size = sim->trail[sim->index];
    sim->index++;
    return size;
}

uint8_t *simulator_replay_packet(Simulator_Control *sim, int size) {
    //  Return NULL in case of error.
    if (size <= 0) { return NULL; }
    if ((sim->index + size) >= (MAX_TRAIL_SIZE + 1) 
        || (sim->index + size) >= (sim->length + 1))  { return NULL; }
    uint8_t *packet = &(sim->trail[sim->index]);
    sim->index = sim->index + size;
    return packet;
}

//  For simulation, fetch the next packet size and next packet content from the trail.
int simulator_simulate_size(Simulator_Control *sim) {
    return simulator_replay_size(sim);
}

uint8_t *simulator_simulate_packet(Simulator_Control *sim, int size) {
    return simulator_replay_packet(sim, size);
}

bool simulator_is_request_complete(Simulator_Control *sim) {
    //  Return true if last SPI command was completed successfully or with error.
    if (!sim->port) return true;
    return spi_is_transceive_complete(sim->port);
}

bool simulator_is_request_successful(Simulator_Control *sim) {
    //  Return true if last SPI command was successful.
    if (!sim->port) return true;
    return spi_is_transceive_successful(sim->port);
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
    if (sim->mode == Simulator_Disabled) { return Simulator_Ok; }  //  If simulator disabled, quit.
    SPI_Control *port = sim->port;
    if (port) { debug_print("sim >> close spi"); debug_println((int) port->id);  }

    //  For Replay Mode, close the SPI port and split the received merged packet into the simulator trail.
    if (port != NULL && sim->mode == Simulator_Replay) { 
        spi_close(port);
        if (simulator_should_replay_merged_trail(sim)) { spi_split_trail(port); }
    }
    if (port) { spi_dump_trail(port); }  //  Dump the trail for debug.
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
    if (port) { port->simulator = NULL; }
    return Simulator_Ok;
}

Simulator_Fails simulator_test(Simulator_Control *sim) {  
    //  For testing only.
    return Simulator_Ok;
}

#endif  //  USE_SIMULATOR
