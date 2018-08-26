# GDB script for restarting STM32 Blue Pill.  This script sends
# commands to an OpenOCD process that's already running to control
# the Blue Pill.

# Set architecture to ARM 32-bit. Needed for gdb-multiarch on Ubuntu.
set architecture arm

# Send GDB commands to OpenOCD, which listens on port 3333.  Extend the timeout.
set remotetimeout 100000
target remote :3333

# Disable all messages.
set verbose off
set complaints 0
set confirm off
set exec-done-display off
show exec-done-display
set trace-commands off
set debug displaced off 
set debug expression 0
set debug frame 0
set debug infrun 0
set debug observer 0
set debug overload 0
set pagination off
set print address off
set print symbol-filename off
set print symbol off
set print pretty off
set print object off
set debug parser off
set debug remote 0

# Print demangled symbols by default.
set print asm-demangle on

# Enable ARM semihosting to show debug console output in OpenOCD console.
monitor arm semihosting enable

# Restart the device.
monitor reset run
quit
