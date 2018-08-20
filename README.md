Arduino sketch to read BME280 I2C temperature, humidity and altitude sensors and send the aggregated sensor data to the Sigfox IoT network (via the Wisol Sigfox module connected on UART).  Supports Sigfox downlink too. Multitasking of the sensors is handled by cocoOS, the portable task scheduling framework: http://www.cocoos.net

Read the tutorials:

- Part 1: _Juggling Arduino Sensors With cocoOS_<br>
    https://medium.com/coinmonks/juggling-arduino-sensors-with-cocoos-403e14ec28be

- Part 2: _Juggling Sigfox Downlink And Arduino Sensors With cocoOS_<br>
    https://medium.com/coinmonks/juggling-sigfox-downlink-and-arduino-sensors-with-cocoos-4594be59bf1b

The code compiles under the Arduino IDE, Arduino Web Editor, and Visual Studio Code with the PlatformIO extension installed. You'll need to install the following libraries:

- `BME280`: Download the `BME280` library by Tyler Glenn from Arduino Library Manager

- `cocoOS_5.0.2`: Download from http://www.cocoos.net/download.html, <br>
    Unzip and move all files in inc and src to top level. <br>
    Replace os_defines.h by our custom cocoOS settings: <br>
    https://github.com/lupyuen/send_altitude_cocoos/blob/master/os_defines.h <br>
    Zip up and add to Arduino IDE or Arduino Web Editor as a library.

Tested with Arduino Uno.

## Create Source File Links For PlatformIO

To compile the project under PlatformIO in Visual Studio Code, the source files
should be symbolically linked into the folder `src`

-----
### For Windows

Open a Command Prompt with Admin permissions. Run the following:

```cmd
git clone https://github.com/lupyuen/send_altitude_cocoos.git
cd send_altitude_cocoos
mkdir lib
mklink libraries lib
mkdir src
cd src
FOR %f IN (..\*.ino ..\*.cpp ..\*.h) DO mklink %~nf%~xf ..\%~nf%~xf
cd ..
```

Using Windows Explorer, copy `cocoOS_5.0.2` source files (`*.h, *.c`) into `send_altitude_cocoos/lib/cocoOS_5.0.2/src`.

Then link `os_defines.h` like this:

```cmd
cd lib\cocoOS_5.0.2\src
del os_defines.h
mklink os_defines.h ..\..\..\os_defines.h
cd ..\..\..
```

-----
### For MacOS and Linux

Open a Command Prompt. Run the following:

```bash
git clone https://github.com/lupyuen/send_altitude_cocoos.git
cd send_altitude_cocoos
mkdir lib
ln -s lib libraries
mkdir src
cd src
ln -s ../*.ino ../*.cpp ../*.h .
cd ..
```

Using Mac Finder, copy `cocoOS_5.0.2` source files (`*.h, *.c`) into `send_altitude_cocoos/lib/cocoOS_5.0.2/src`.

Then link `os_defines.h` like this:

```bash
cd lib/cocoOS_5.0.2/src
rm os_defines.h
ln -s ../../../os_defines.h .
cd ../../..
```

To allow Arduino IDE and PlatformIO to share the same cocoOS library:

```bash
ln -s ~/send_altitude_cocoos/lib/cocoOS_5.0.2/src/ ~/Documents/Arduino/libraries/cocoOS_5.0.2
```

Arduino IDE does not compile if send_altitude_cocoos/src contains any files. Remove the links before compiling in Arduino IDE.

Or use these 2 helper scripts:

- `scripts/linksrc.sh`: Links the source files to `src` folder and launches Visual Studio Code

- `scripts/unlinksrc.sh`: Unlinks the source files from `src` folder and launches Arduino IDE

-----
## Downlink Server Support

(From https://backend.sigfox.com/apidocs/callback)

When a message needs to be acknowledged, the callback selected for the downlink data must 
send data in the HTTP response. It must contain the 8 bytes data that will be sent to the device 
asking for acknowledgment. The data is JSON formatted, and must be structured as the following :

```json
  { "YOUR_DEVICE_ID" : { "downlinkData" : "deadbeefcafebabe" } }    
```
With YOUR_DEVICE_ID being replaced by the corresponding device id, in hexadecimal format, up to 8 digits. The downlink data must be 8 bytes in hexadecimal format.  For example:

```json
  { "002C2EA1" : { "downlinkData" : "0102030405060708" } }
```

In AWS Lambda you can create a function to return the downlink like this (use API Gateway to expose as REST URL):

```javascript
//  Demonstrates a simple Sigfox Downlink HTTP endpoint using AWS Lambda and API Gateway. 
exports.handler = (event, context, callback) => {
    console.log('Received event:', JSON.stringify(event, null, 2));    
    //  Get device ID from the JSON string in the POST request, which looks like:
    //  {"device": "2C2EA1", "data": "0102030405060708090a0b0c", ...}
    const body = event.body ? JSON.parse(event.body) : {};    
    const device = body.device || '002C2EA1';  //  Default device ID to 002C2EA1 if missing.

    //  TODO: This is the 8-byte downlink data to be returned, in 16 hex digits.
    const data = 'fedcba9876543210';
  
    //  Compose the JSON downlink response. [device] will be replaced by the device ID.
    const res = { [device]: { 
        "downlinkData" : data
    } };
    //  Return the JSON response to API Gateway.
    return callback(null, {
        statusCode: 200,
        body: JSON.stringify(res),
        headers: { 'Content-Type': 'application/json' },
    });
};
```

Note: Only the first 4 downlinks per day are guaranteed.  You may send more downlink
requests but they are not guaranteed.

-----
## Build for STM32 Blue Pill with PlatformIO

To build for STM32 Blue Pill on Visual Studio Code and PlatformIO, edit `platformio.ini` and uncomment the section `[env:bluepill_f103c8]`

-----
## Source Files

[`main.cpp`](main.cpp): Main program. The Arduino application starts here in function `main()`

[`send_altitude_cocoos.ino`](send_altitude_cocoos.ino): Arduino sketch with no code. Arduino IDE requires this file to be present.

-----
### Network Code

[`sigfox.h`](sigfox.h): Message definitions for the Sigfox network.

[`wisol.cpp`](wisol.cpp), [`wisol.h`](wisol.h): Network Task to receive and aggregate sensor data from Sensor Task and send to the Sigfox network.

[`uart.cpp`](uart.cpp), [`uart.h`](uart.h): UART Task to send and receive network messages to/from the Sigfox network via a Wisol Sigfox module connected to the serial port.

[`aggregate.cpp`](aggregate.cpp), [`aggregate.h`](aggregate.h): Aggregate sensor data from multiple Sensor Tasks and determine if the aggregated sensor data should be sent to the network now.

-----
### Sensor Code

[`sensor.cpp`](sensor.cpp), [`sensor.h`](sensor.h): Sensor Task to poll sensor data repeatedly

[`bme280.cpp`](bme280.cpp), [`bme280.h`](bme280.h): Interface to BME280 I2C module

[`alt_sensor.cpp`](alt_sensor.cpp), [`alt_sensor.h`](alt_sensor.h): Altitude sensor (BME280 I2C)

[`humid_sensor.cpp`](humid_sensor.cpp), [`humid_sensor.h`](humid_sensor.h): Humidity sensor (BME280 I2C)

[`temp_sensor.cpp`](temp_sensor.cpp), [`temp_sensor.h`](temp_sensor.h): Temperature sensor (BME280 I2C)

[`gyro_sensor.cpp`](gyro_sensor.cpp), [`gyro_sensor.h`](gyro_sensor.h): Gyroscope sensor (simulated)

-----
### Other Code

[`cocoos_cpp.h`](cocoos_cpp.h): Workaround for cross initialisation problem when using cocoOS from C++

[`display.cpp`](display.cpp), [`display.h`](display.h): Display Task to display sensor data

-----
### Build and Configuration Files

[`.pioenvs`](.pioenvs): Build files for PlatformIO

[`.travis.yml`](.travis.yml): Travis CI file (outdated)

[`.vscode`](.vscode): Configuration files for Visual Studio Code

[`platformio.ini`](platformio.ini): Configuration file for PlatformIO

[`sketch.json`](sketch.json): Arduino sketch details generated by Arduino Web Editor

[`cmake`](cmake), [`CMakeLists.txt`](CMakeLists.txt): CMake files for building in JetBrains CLion

-----
## Sample Log

-----------------------------------------------------------------------------------
### Uplink Only

```text
> Executing task in folder send_altitude_cocoos: platformio device monitor <

--- Miniterm on /dev/cu.usbmodem1411  9600,8,N,1 ---
--- Quit: Ctrl+C | Menu: Ctrl+T | Help: Ctrl+T followed by Ctrl+H ---
----arduino_setup
Create semaphore
BME280 OK
net >> Wait for net
net >> Got net
>> ATS410=0[0x0d]
<< OK[0x0d]
>> AT$I=10[0x0d]
<< 002C2EA1[0x0d]
 - wisol.getID: 002C2EA1
>> AT$I=11[0x0d]
<< 5BEB8CG64E869BD1[0x0d]
 - wisol.getPAC: 5BEB8CG64E869BD1
net >> Release net
tmp >> poll_sensor
hmd >> poll_sensor
alt >> poll_sensor
alt << Recv sensor data 114.54
tmp >> poll_sensor
tmp << Recv sensor data 34.15
hmd >> poll_sensor
hmd << Recv sensor data 56.09
alt >> poll_sensor
alt << Recv sensor data 112.06
tmp >> poll_sensor
tmp << Recv sensor data 34.13
hmd >> poll_sensor
hmd << Recv sensor data 56.08
alt >> poll_sensor
alt << Recv sensor data 114.19
tmp >> poll_sensor
tmp << Recv sensor data 34.13
hmd >> poll_sensor
hmd << Recv sensor data 56.08
alt >> poll_sensor
alt << Recv sensor data 116.10
tmp >> poll_sensor
tmp << Recv sensor data 34.13
hmd >> poll_sensor
hmd << Recv sensor data 56.09
alt >> poll_sensor
alt << Recv sensor data 115.15
agg >> Send 0000034105601151
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,6[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0000034105601151[0x0d]
tmp >> poll_sensor
tmp << Recv sensor data 34.12
hmd >> poll_sensor
hmd << Recv sensor data 56.09
alt >> poll_sensor
alt << Recv sensor data 113.02
<< OK[0x0d]
net >> Pending response
tmp >> poll_sensor
tmp << Recv sensor data 34.11
hmd >> poll_sensor
hmd << Recv sensor data 56.10
alt >> poll_sensor
alt << Recv sensor data 113.74
tmp >> poll_sensor
tmp << Recv sensor data 34.10
hmd >> poll_sensor
hmd << Recv sensor data 56.08
alt >> poll_sensor
alt << Recv sensor data 113.16
tmp >> poll_sensor
tmp << Recv sensor data 34.11
hmd >> poll_sensor
hmd << Recv sensor data 56.10
alt >> poll_sensor
alt << Recv sensor data 114.22
tmp >> poll_sensor
tmp << Recv sensor data 34.11
hmd >> poll_sensor
hmd << Recv sensor data 56.10
alt >> poll_sensor
alt << Recv sensor data 114.22
tmp >> poll_sensor
tmp << Recv sensor data 34.11
agg >> Send 0001034105601142
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,3[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0001034105601142[0x0d]
hmd >> poll_sensor
hmd << Recv sensor data 56.08
alt >> poll_sensor
alt << Recv sensor data 113.28
tmp >> poll_sensor
tmp << Recv sensor data 34.09
<< OK[0x0d]
net >> Pending response
hmd >> poll_sensor
hmd << Recv sensor data 56.08
alt >> poll_sensor
alt << Recv sensor data 114.11
tmp >> poll_sensor
tmp << Recv sensor data 34.09
hmd >> poll_sensor
hmd << Recv sensor data 56.07
alt >> poll_sensor
alt << Recv sensor data 112.70
tmp >> poll_sensor
tmp << Recv sensor data 34.08
hmd >> poll_sensor
hmd << Recv sensor data 56.08
alt >> poll_sensor
alt << Recv sensor data 112.70
tmp >> poll_sensor
tmp << Recv sensor data 34.08
hmd >> poll_sensor
hmd << Recv sensor data 56.07
alt >> poll_sensor
alt << Recv sensor data 114.36
tmp >> poll_sensor
tmp << Recv sensor data 34.08
hmd >> poll_sensor
hmd << Recv sensor data 56.05
agg >> Send 0002034005601143
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,0[0x0d]
>> AT$RC[0x0d]
<< OK[0x0d]
net >> Release net
>␂
Q�SF=0002034005601143[0x0d]
alt >> poll_sensor
alt << Recv sensor data 114.84
tmp >> poll_sensor
tmp << Recv sensor data 34.08
hmd >> poll_sensor
hmd << Recv sensor data 56.07
<< OK[0x0d]
net >> Pending response
alt >> poll_sensor
alt << Recv sensor data 114.36
tmp >> poll_sensor
tmp << Recv sensor data 34.08
hmd >> poll_sensor
hmd << Recv sensor data 56.06
alt >> poll_sensor
alt << Recv sensor data 114.01
tmp >> poll_sensor
tmp << Recv sensor data 34.08
hmd >> poll_sensor
hmd << Recv sensor data 56.06
alt >> poll_sensor
alt << Recv sensor data 113.77
tmp >> poll_sensor
tmp << Recv sensor data 34.07
hmd >> poll_sensor
hmd << Recv sensor data 56.06
alt >> poll_sensor
alt << Recv sensor data 113.18
tmp >> poll_sensor
tmp << Recv sensor data 34.06
hmd >> poll_sensor
hmd << Recv sensor data 56.06
alt >> poll_sensor
alt << Recv sensor data 113.77
agg >> Send 0003034005601137
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,3[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0003034005601137[0x0d]
tmp >> poll_sensor
tmp << Recv sensor data 34.07
hmd >> poll_sensor
hmd << Recv sensor data 56.06
alt >> poll_sensor
alt << Recv sensor data 113.77
<< OK[0x0d]
net >> Pending response
tmp >> poll_sensor
tmp << Recv sensor data 34.07
hmd >> poll_sensor
hmd << Recv sensor data 56.04
alt >> poll_sensor
alt << Recv sensor data 115.08
tmp >> poll_sensor
tmp << Recv sensor data 34.07
hmd >> poll_sensor
hmd << Recv sensor data 56.06
alt >> poll_sensor
alt << Recv sensor data 114.60
tmp >> poll_sensor
tmp << Recv sensor data 34.06
hmd >> poll_sensor
hmd << Recv sensor data 56.07
alt >> poll_sensor
alt << Recv sensor data 115.08
tmp >> poll_sensor
tmp << Recv sensor data 34.06
hmd >> poll_sensor
hmd << Recv sensor data 56.04
alt >> poll_sensor
alt << Recv sensor data 113.18
tmp >> poll_sensor
tmp << Recv sensor data 34.07
agg >> Send 0004034005601131
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,0[0x0d]
>> AT$RC[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0004034005601131[0x0d]
hmd >> poll_sensor
hmd << Recv sensor data 56.03
alt >> poll_sensor
alt << Recv sensor data 113.65
tmp >> poll_sensor
tmp << Recv sensor data 34.06
<< OK[0x0d]
net >> Pending response
hmd >> poll_sensor
hmd << Recv sensor data 56.04
alt >> poll_sensor
alt << Recv sensor data 114.48


```

-----------------------------------------------------------------------------------
### Uplink and Downlink

```text
> Executing task in folder send_altitude_cocoos: platformio device monitor <

--- Miniterm on /dev/cu.usbmodem1411  9600,8,N,1 ---
--- Quit: Ctrl+C | Menu: Ctrl+T | Help: Ctrl+T followed by Ctrl+H ---
----arduino_setup
Create semaphore
BME280 OK
net >> Wait for net
net >> Got net
>> ATS410=0[0x0d]
<< OK[0x0d]
>> AT$I=10[0x0d]
<< 002C2EA1[0x0d]
 - wisol.getID: 002C2EA1
>> AT$I=11[0x0d]
<< 5BEB8CG64E869BD1[0x0d]
 - wisol.getPAC: 5BEB8CG64E869BD1
net >> Release net
tmp >> poll_sensor
hmd >> poll_sensor
alt >> poll_sensor
alt << Recv sensor data 236.13
tmp >> poll_sensor
tmp << Recv sensor data 34.80
hmd >> poll_sensor
hmd << Recv sensor data 50.82
alt >> poll_sensor
alt << Recv sensor data 236.73
tmp >> poll_sensor
tmp << Recv sensor data 34.77
hmd >> poll_sensor
hmd << Recv sensor data 50.87
alt >> poll_sensor
alt << Recv sensor data 235.79
tmp >> poll_sensor
tmp << Recv sensor data 34.73
hmd >> poll_sensor
hmd << Recv sensor data 50.88
alt >> poll_sensor
alt << Recv sensor data 236.75
tmp >> poll_sensor
tmp << Recv sensor data 34.73
hmd >> poll_sensor
hmd << Recv sensor data 50.91
alt >> poll_sensor
alt << Recv sensor data 238.42
agg >> Send 0000034705092384
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,6[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0000034705092384,1[0x0d]
tmp >> poll_sensor
tmp << Recv sensor data 34.70
hmd >> poll_sensor
hmd << Recv sensor data 50.95
alt >> poll_sensor
alt << Recv sensor data 236.06
tmp >> poll_sensor
tmp << Recv sensor data 34.67
hmd >> poll_sensor
hmd << Recv sensor data 51.00
alt >> poll_sensor
alt << Recv sensor data 237.49
tmp >> poll_sensor
tmp << Recv sensor data 34.65
hmd >> poll_sensor
hmd << Recv sensor data 51.02
alt >> poll_sensor
alt << Recv sensor data 236.79
tmp >> poll_sensor
tmp << Recv sensor data 34.62
hmd >> poll_sensor
hmd << Recv sensor data 51.05
alt >> poll_sensor
alt << Recv sensor data 238.22
tmp >> poll_sensor
tmp << Recv sensor data 34.62
hmd >> poll_sensor
hmd << Recv sensor data 51.08
alt >> poll_sensor
alt << Recv sensor data 239.54
tmp >> poll_sensor
tmp << Recv sensor data 34.58
hmd >> poll_sensor
hmd << Recv sensor data 51.10
alt >> poll_sensor
alt << Recv sensor data 238.25
tmp >> poll_sensor
tmp << Recv sensor data 34.56
hmd >> poll_sensor
hmd << Recv sensor data 51.13
alt >> poll_sensor
alt << Recv sensor data 237.80
tmp >> poll_sensor
tmp << Recv sensor data 34.54
hmd >> poll_sensor
hmd << Recv sensor data 51.16
alt >> poll_sensor
alt << Recv sensor data 238.75
tmp >> poll_sensor
tmp << Recv sensor data 34.52
hmd >> poll_sensor
hmd << Recv sensor data 51.16
alt >> poll_sensor
alt << Recv sensor data 236.63
tmp >> poll_sensor
tmp << Recv sensor data 34.51
hmd >> poll_sensor
hmd << Recv sensor data 51.23
alt >> poll_sensor
alt << Recv sensor data 239.35
tmp >> poll_sensor
tmp << Recv sensor data 34.49
hmd >> poll_sensor
hmd << Recv sensor data 51.24
alt >> poll_sensor
alt << Recv sensor data 241.49
tmp >> poll_sensor
tmp << Recv sensor data 34.47
hmd >> poll_sensor
hmd << Recv sensor data 51.18
alt >> poll_sensor
alt << Recv sensor data 240.79
<< (Timeout)
<< ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT[0x0d][0x0a]
***** uart.sendData: Error: Unknown response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT

net >> Pending response
***** Error: network_task Failed, response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
 - process_downlink_msg (Failed): ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
tmp >> poll_sensor
tmp << Recv sensor data 34.45
hmd >> poll_sensor
hmd << Recv sensor data 51.19
alt >> poll_sensor
alt << Recv sensor data 241.98
tmp >> poll_sensor
tmp << Recv sensor data 34.43
hmd >> poll_sensor
hmd << Recv sensor data 51.20
alt >> poll_sensor
alt << Recv sensor data 239.39
tmp >> poll_sensor
tmp << Recv sensor data 34.41
hmd >> poll_sensor
hmd << Recv sensor data 51.24
alt >> poll_sensor
alt << Recv sensor data 240.35
tmp >> poll_sensor
tmp << Recv sensor data 34.39
hmd >> poll_sensor
hmd << Recv sensor data 51.30
alt >> poll_sensor
alt << Recv sensor data 241.78
tmp >> poll_sensor
tmp << Recv sensor data 34.36
agg >> Send 0001034305122417
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,6[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0001034305122417,1[0x0d]
hmd >> poll_sensor
hmd << Recv sensor data 51.33
alt >> poll_sensor
alt << Recv sensor data 239.66
tmp >> poll_sensor
tmp << Recv sensor data 34.34
hmd >> poll_sensor
hmd << Recv sensor data 51.36
alt >> poll_sensor
alt << Recv sensor data 241.09
tmp >> poll_sensor
tmp << Recv sensor data 34.31
hmd >> poll_sensor
hmd << Recv sensor data 51.43
alt >> poll_sensor
alt << Recv sensor data 241.81
tmp >> poll_sensor
tmp << Recv sensor data 34.28
hmd >> poll_sensor
hmd << Recv sensor data 51.50
alt >> poll_sensor
alt << Recv sensor data 239.94
tmp >> poll_sensor
tmp << Recv sensor data 34.26
hmd >> poll_sensor
hmd << Recv sensor data 51.54
alt >> poll_sensor
alt << Recv sensor data 241.13
tmp >> poll_sensor
tmp << Recv sensor data 34.24
hmd >> poll_sensor
hmd << Recv sensor data 51.58
alt >> poll_sensor
alt << Recv sensor data 240.67
tmp >> poll_sensor
tmp << Recv sensor data 34.21
hmd >> poll_sensor
hmd << Recv sensor data 51.61
alt >> poll_sensor
alt << Recv sensor data 240.45
tmp >> poll_sensor
tmp << Recv sensor data 34.19
hmd >> poll_sensor
hmd << Recv sensor data 51.66
alt >> poll_sensor
alt << Recv sensor data 239.75
tmp >> poll_sensor
tmp << Recv sensor data 34.15
hmd >> poll_sensor
hmd << Recv sensor data 51.69
alt >> poll_sensor
alt << Recv sensor data 239.77
tmp >> poll_sensor
tmp << Recv sensor data 34.13
hmd >> poll_sensor
hmd << Recv sensor data 51.75
alt >> poll_sensor
alt << Recv sensor data 238.82
tmp >> poll_sensor
tmp << Recv sensor data 34.10
hmd >> poll_sensor
hmd << Recv sensor data 51.80
alt >> poll_sensor
alt << Recv sensor data 241.09
tmp >> poll_sensor
tmp << Recv sensor data 34.09
hmd >> poll_sensor
hmd << Recv sensor data 51.82
alt >> poll_sensor
alt << Recv sensor data 240.05
tmp >> poll_sensor
tmp << Recv sensor data 34.06
<< (Timeout)
<< ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT[0x0d][0x0a]
***** uart.sendData: Error: Unknown response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT

net >> Pending response
***** Error: network_task Failed, response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
 - process_downlink_msg (Failed): ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
hmd >> poll_sensor
hmd << Recv sensor data 51.84
alt >> poll_sensor
alt << Recv sensor data 239.23
tmp >> poll_sensor
tmp << Recv sensor data 34.05
hmd >> poll_sensor
hmd << Recv sensor data 51.88
alt >> poll_sensor
alt << Recv sensor data 241.00
tmp >> poll_sensor
tmp << Recv sensor data 34.02
hmd >> poll_sensor
hmd << Recv sensor data 51.87
alt >> poll_sensor
alt << Recv sensor data 241.96
tmp >> poll_sensor
tmp << Recv sensor data 34.01
hmd >> poll_sensor
hmd << Recv sensor data 51.87
alt >> poll_sensor
alt << Recv sensor data 240.44
tmp >> poll_sensor
tmp << Recv sensor data 33.98
hmd >> poll_sensor
hmd << Recv sensor data 51.88
agg >> Send 0002033905182404
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,6[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
�␂
Q�SF=0002033905182404,1[0x0d]
alt >> poll_sensor
alt << Recv sensor data 242.22
tmp >> poll_sensor
tmp << Recv sensor data 33.96
hmd >> poll_sensor
hmd << Recv sensor data 51.91
alt >> poll_sensor
alt << Recv sensor data 241.52
tmp >> poll_sensor
tmp << Recv sensor data 33.93
hmd >> poll_sensor
hmd << Recv sensor data 51.96
alt >> poll_sensor
alt << Recv sensor data 241.89
tmp >> poll_sensor
tmp << Recv sensor data 33.91
hmd >> poll_sensor
hmd << Recv sensor data 52.00
alt >> poll_sensor
alt << Recv sensor data 239.77
tmp >> poll_sensor
tmp << Recv sensor data 33.89
hmd >> poll_sensor
hmd << Recv sensor data 52.06
alt >> poll_sensor
alt << Recv sensor data 242.62
tmp >> poll_sensor
tmp << Recv sensor data 33.87
hmd >> poll_sensor
hmd << Recv sensor data 52.14
alt >> poll_sensor
alt << Recv sensor data 241.93
tmp >> poll_sensor
tmp << Recv sensor data 33.84
hmd >> poll_sensor
hmd << Recv sensor data 52.22
alt >> poll_sensor
alt << Recv sensor data 240.64
tmp >> poll_sensor
tmp << Recv sensor data 33.82
hmd >> poll_sensor
hmd << Recv sensor data 52.23
alt >> poll_sensor
alt << Recv sensor data 240.77
<< OK[0x0d][0x0a]RX=FE DC BA 98 76 54 32 10[0x0d]
net >> Pending response
 - process_downlink_msg: FEDCBA9876543210
tmp >> poll_sensor
tmp << Recv sensor data 33.79
hmd >> poll_sensor
hmd << Recv sensor data 52.22
alt >> poll_sensor
alt << Recv sensor data 241.73
tmp >> poll_sensor
tmp << Recv sensor data 33.77
hmd >> poll_sensor
hmd << Recv sensor data 52.30
alt >> poll_sensor
alt << Recv sensor data 242.09
tmp >> poll_sensor
tmp << Recv sensor data 33.76
hmd >> poll_sensor
hmd << Recv sensor data 52.36
alt >> poll_sensor
alt << Recv sensor data 241.39
tmp >> poll_sensor
tmp << Recv sensor data 33.73
hmd >> poll_sensor
hmd << Recv sensor data 52.38
alt >> poll_sensor
alt << Recv sensor data 243.42
tmp >> poll_sensor
tmp << Recv sensor data 33.71
agg >> Send 0003033705232434
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,5[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0003033705232434,1[0x0d]
hmd >> poll_sensor
hmd << Recv sensor data 52.43
alt >> poll_sensor
alt << Recv sensor data 241.66
tmp >> poll_sensor
tmp << Recv sensor data 33.68
hmd >> poll_sensor
hmd << Recv sensor data 52.52
alt >> poll_sensor
alt << Recv sensor data 241.79
tmp >> poll_sensor
tmp << Recv sensor data 33.67
hmd >> poll_sensor
hmd << Recv sensor data 52.59
alt >> poll_sensor
alt << Recv sensor data 242.15
tmp >> poll_sensor
tmp << Recv sensor data 33.63
hmd >> poll_sensor
hmd << Recv sensor data 52.64
alt >> poll_sensor
alt << Recv sensor data 242.52
tmp >> poll_sensor
tmp << Recv sensor data 33.62
hmd >> poll_sensor
hmd << Recv sensor data 52.63
alt >> poll_sensor
alt << Recv sensor data 241.00
tmp >> poll_sensor
tmp << Recv sensor data 33.60
hmd >> poll_sensor
hmd << Recv sensor data 52.67
alt >> poll_sensor
alt << Recv sensor data 241.37
tmp >> poll_sensor
tmp << Recv sensor data 33.58
hmd >> poll_sensor
hmd << Recv sensor data 52.72
alt >> poll_sensor
alt << Recv sensor data 242.09
tmp >> poll_sensor
tmp << Recv sensor data 33.56
hmd >> poll_sensor
hmd << Recv sensor data 52.77
alt >> poll_sensor
alt << Recv sensor data 241.63
tmp >> poll_sensor
tmp << Recv sensor data 33.54
hmd >> poll_sensor
hmd << Recv sensor data 52.76
alt >> poll_sensor
alt << Recv sensor data 240.11
tmp >> poll_sensor
tmp << Recv sensor data 33.51
hmd >> poll_sensor
hmd << Recv sensor data 52.76
alt >> poll_sensor
alt << Recv sensor data 239.41
tmp >> poll_sensor
tmp << Recv sensor data 33.49
hmd >> poll_sensor
hmd << Recv sensor data 52.81
alt >> poll_sensor
alt << Recv sensor data 241.79
tmp >> poll_sensor
tmp << Recv sensor data 33.48
hmd >> poll_sensor
hmd << Recv sensor data 52.86
alt >> poll_sensor
alt << Recv sensor data 240.85
tmp >> poll_sensor
tmp << Recv sensor data 33.46
<< (Timeout)
<< ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT[0x0d][0x0a]
***** uart.sendData: Error: Unknown response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT

net >> Pending response
***** Error: network_task Failed, response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
 - process_downlink_msg (Failed): ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
hmd >> poll_sensor
hmd << Recv sensor data 52.88
alt >> poll_sensor
alt << Recv sensor data 240.50
tmp >> poll_sensor
tmp << Recv sensor data 33.44
hmd >> poll_sensor
hmd << Recv sensor data 52.93
alt >> poll_sensor
alt << Recv sensor data 241.70
tmp >> poll_sensor
tmp << Recv sensor data 33.43
hmd >> poll_sensor
hmd << Recv sensor data 52.97

```
