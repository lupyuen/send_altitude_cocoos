# Unlink the source files in folder `src` so that Arduino IDE can compile.

rm src/*.ino src/*.cpp src/*.h

# Launch Arduino IDE.
open send_altitude_cocoos.ino
