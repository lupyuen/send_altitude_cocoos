Arduino sketch to read BME280 I2C temperature, humidity and altitude sensors and send the aggregated sensor data to the Sigfox IoT network (via the Wisol Sigfox module connected on UART).  Supports Sigfox downlink too. Multitasking of the sensors is handled by cocoOS, the portable task scheduling framework: http://www.cocoos.net

Read the tutorial: (to be updated with network details) https://medium.com/coinmonks/juggling-arduino-sensors-with-cocoos-403e14ec28be

The code compiles under the Arduino IDE, Arduino Web Editor, and Visual Studio Code with the PlatformIO extension installed. You'll need to install the following libraries:

- `BME280`: Download the `BME280` library by Tyler Glenn from Arduino Library Manager

- `cocoOS_5.0.1`: Download from http://www.cocoos.net/download.html, 
    unzip and move all files in `inc` and `src` to top level.
    Zip up and add to Arduino IDE as library.

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
mkdir src
cd src
FOR %f IN (..\*.ino ..\*.cpp ..\*.h) DO mklink %~nf%~xf ..\%~nf%~xf
cd ..
```

Using Windows Explorer, copy `cocoOS_5.0.1` source files (`*.h, *.c`) into `send_altitude_cocoos/lib/cocoOS_5.0.1/src`.

Then link `os_defines.h` like this:

```cmd
cd lib\cocoOS_5.0.1\src
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
mkdir src
cd src
ln -s ../*.ino ../*.cpp ../*.h .
cd ..
```

Using Mac Finder, copy `cocoOS_5.0.1` source files (`*.h, *.c`) into `send_altitude_cocoos/lib/cocoOS_5.0.1/src`.

Then link `os_defines.h` like this:

```bash
cd lib/cocoOS_5.0.1/src
rm os_defines.h
ln -s ../../../os_defines.h .
cd ../../..
```

To allow Arduino IDE and PlatformIO to share the same cocoOS library:

```bash
ln -s ~/send_altitude_cocoos/lib/cocoOS_5.0.1/src/ ~/Documents/Arduino/libraries/cocoOS_5.0.1
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
//  Demonstrates a simple Sigfox downlink HTTP endpoint using API Gateway. 
//  With API Gateway you have full access to the request and response payload, 
//  including headers and status code.

exports.handler = (event, context, callback) => {
    console.log('Received event:', JSON.stringify(event, null, 2));
    
    //  Get device ID from body.
    //  event.body = "{\r\n\"device\" : \"2C2EA1\", \r\n\"data\" : \"0102030405060708090a0b0c\", ...}"
    const body = event.body ? JSON.parse(event.body) : {};
    
    //  body = {device: "2C2EA1", data: "0102030405060708090a0b0c", ...}
    const device = body.device || '002C2EA1';

    //  Downlink data to be returned.    
    const data = 'fedcba9876543210';
    const res = { 
        [device]: { 
            "downlinkData" : data
        }
    };
    return callback(null, {
        statusCode: 200,
        body: JSON.stringify(res),
        headers: {
            'Content-Type': 'application/json',
        },
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

-----
## Sample Log

```text
----arduino_setup
Create semaphore
BME280 OK
net >> Wait for net
net >> Got net
>> ATS410=0[0x0d]
<< OK[0x0d]
-----i1 / i2: 0 / 0
>> AT$I=10[0x0d]
<< 002C2EA1[0x0d]
-----i1 / i2: 0 / 0
 - wisol.getID: 002C2EA1
>> AT$I=11[0x0d]
<< 5BEB8CG64E869BD1[0x0d]
-----i1 / i2: 0 / 0
 - wisol.getPAC: 5BEB8CG64E869BD1
net >> Release net
tmp >> poll_sensor
hmd >> poll_sensor
alt >> poll_sensor
alt << Recv sensor data 33.68
tmp >> poll_sensor
tmp << Recv sensor data 31.87
hmd >> poll_sensor
hmd << Recv sensor data 61.78
alt >> poll_sensor
alt << Recv sensor data 33.43
tmp >> poll_sensor
tmp << Recv sensor data 31.87
hmd >> poll_sensor
hmd << Recv sensor data 61.78
alt >> poll_sensor
alt << Recv sensor data 33.60
tmp >> poll_sensor
tmp << Recv sensor data 31.86
hmd >> poll_sensor
hmd << Recv sensor data 61.80
alt >> poll_sensor
alt << Recv sensor data 33.53
tmp >> poll_sensor
tmp << Recv sensor data 31.86
hmd >> poll_sensor
hmd << Recv sensor data 61.80
alt >> poll_sensor
alt << Recv sensor data 33.28
agg >> Send 000318618332
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,6[0x0d]
-----i1 / i2: 0 / 0
>> AT[0x0d]
<< OK[0x0d]
-----i1 / i2: 0 / 0
>> AT$SF=303030333138363138333332,1[0x0d]
<< OK[0x0d][0x0a]RX=FE DC BA 98 76 54 32 10[0x0d]
-----i1 / i2: 3679 / 0
net >> Release net
 - processDownlinkMsg: FEDCBA9876543210
tmp >> poll_sensor
hmd >> poll_sensor

```
