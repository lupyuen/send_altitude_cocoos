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
cd send_altitude_cocoos
mkdir src
cd src
FOR %f IN (..\*.ino ..\*.cpp ..\*.c ..\*.h) DO mklink %~nf%~xf ..\%~nf%~xf
```

For MacOS and Linux: Open a Command Prompt. Run the following:

```bash
cd send_altitude_cocoos
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
Create semaphore
BME280 OK
tmp >> Wait for semaphore
tmp >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg
tmp >> Release semaphore
tmp >> Wait interval
hmd >> Wait for semaphore
hmd >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg
hmd >> Release semaphore
hmd >> Wait interval
alt >> Wait for semaphore
alt >> Got semaphore
alt >> poll_sensor
alt >> Send msg
alt >> Release semaphore
alt >> Wait interval
gyr >> Wait for semaphore
gyr >> Got semaphore
gyr >> poll_sensor
gyr >> Send msg
gyr >> Release semaphore
gyr >> Wait interval
tmp:            27.5
hmd:            61.8
alt:            14.2
gyr:            30.7,           8.8,            48.0
tmp >> Wait for semaphore
tmp >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg
tmp >> Release semaphore
tmp >> Wait interval
tmp:            27.4
hmd >> Wait for semaphore
hmd >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg
hmd >> Release semaphore
hmd >> Wait interval
alt >> Wait for semaphore
alt >> Got semaphore
alt >> poll_sensor
alt >> Send msg
alt >> Release semaphore
alt >> Wait interval
gyr >> Wait for semaphore
gyr >> Got semaphore
gyr >> poll_sensor
gyr >> Send msg
gyr >> Release semaphore
gyr >> Wait interval
hmd:            61.8
alt:            13.5
gyr:            11.3,           21.0,           24.0
tmp >> Wait for semaphore
tmp >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg
tmp >> Release semaphore
tmp >> Wait interval
tmp:            27.4
hmd >> Wait for semaphore
hmd >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg
hmd >> Release semaphore
hmd >> Wait interval
alt >> Wait for semaphore
alt >> Got semaphore
alt >> poll_sensor
alt >> Send msg
alt >> Release semaphore
alt >> Wait interval
gyr >> Wait for semaphore
gyr >> Got semaphore
gyr >> poll_sensor
gyr >> Send msg
gyr >> Release semaphore
gyr >> Wait interval
hmd:            61.8
alt:            13.1
gyr:            30.0,           5.8,            9.8
tmp >> Wait for semaphore
tmp >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg
tmp >> Release semaphore
tmp >> Wait interval
tmp:            27.4
hmd >> Wait for semaphore
hmd >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg
hmd >> Release semaphore
hmd >> Wait interval
alt >> Wait for semaphore
alt >> Got semaphore
alt >> poll_sensor
alt >> Send msg
alt >> Release semaphore
alt >> Wait interval
gyr >> Wait for semaphore
gyr >> Got semaphore
gyr >> poll_sensor
gyr >> Send msg
gyr >> Release semaphore
gyr >> Wait interval
hmd:            61.8
alt:            13.1
gyr:            10.1,           29.6,           44.5
tmp >> Wait for semaphore
tmp >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg
tmp >> Release semaphore
tmp >> Wait interval
tmp:            27.4
hmd >> Wait for semaphore
hmd >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg
hmd >> Release semaphore
hmd >> Wait interval
alt >> Wait for semaphore
alt >> Got semaphore
alt >> poll_sensor
alt >> Send msg
alt >> Release semaphore
alt >> Wait interval
gyr >> Wait for semaphore
gyr >> Got semaphore
gyr >> poll_sensor
gyr >> Send msg
gyr >> Release semaphore
gyr >> Wait interval
hmd:            61.8
alt:            13.5
gyr:            24.7,           2.2,            18.2

```
