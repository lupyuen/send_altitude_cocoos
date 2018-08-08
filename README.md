Arduino sketch to read BME280 temperature, humidity and altitude sensors and process the data concurrently.  Multitasking of the sensors
is handled by cocoOS, the portable scheduling task framework: http://www.cocoos.net

Libraries needed to compile with Arduino IDE or Arduino Web Editor:

- `Time`: Download `Time` from Arduino Library Manager

- `BME280`: Download `BME280` from Arduino Library Manager

- `cocoOS_5.0.1`: Download from http://www.cocoos.net/download.html, 
    unzip and move all files in `inc` and `src` to top level.
    Zip up and add to Arduino IDE as library.

Tested with Arduino Uno.

## Create Source File Links For PlatformIO

To compile the project under PlatformIO in Visual Studio Code, the source files
should be symbolically linked into the folder `src`

For Windows: Open a Command Prompt with Admin permissions. Run the following:

```cmd
cd send-altitude-cocoos
mkdir src
cd src
FOR %f IN (..\*.ino ..\*.cpp ..\*.c ..\*.h) DO mklink %~nf%~xf ..\%~nf%~xf
```

For MacOS and Linux: Open a Command Prompt. Run the following:

```bash
cd send-altitude-cocoos
mkdir src
cd src
ln -s ../*.ino .
ln -s ../*.cpp .
ln -s ../*.c .
ln -s ../*.h .
```

## Sample Log

```text
----arduino_setup
init_display
Create semaphore
tmp >> Wait for semaphore
tmp >> Got semaphore
tmp >> Send msg
tmp >> Release semaphore
tmp >> Wait interval
gyr >> Wait for semaphore
gyr >> Got semaphore
gyr >> Send msg
gyr >> Release semaphore
gyr >> Wait interval
tmp:            19.2
gyr:            39.0,           31.0,           14.0
tmp >> Wait for semaphore
tmp >> Got semaphore
tmp >> Send msg
tmp >> Release semaphore
tmp >> Wait interval
tmp:            12.3
gyr >> Wait for semaphore
gyr >> Got semaphore
gyr >> Send msg
gyr >> Release semaphore
gyr >> Wait interval
gyr:            40.0,           0.0,            8.0

```
