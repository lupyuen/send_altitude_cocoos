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
mklink ..\..\..\os_defines.h os_defines.h
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
> Executing task in folder send_altitude_cocoos: platformio device monitor <

--- Miniterm on /dev/cu.usbmodem1411  9600,8,N,1 ---
--- Quit: Ctrl+C | Menu: Ctrl+T | Help: Ctrl+T followed by Ctrl+H ---
ata: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$I=10
>> AT$I=10
␀< 002C=0
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$I=10
>> AT$I=10
<< 002C----arduino_setup
Create semaphore
BME280 OK
 - wisol.getCmdBegin
 - uart.sendData: ATS410=0
tmp >> Wait for semaphore
hmd >> Wait for semaphore
alt >> Wait for semaphore
␀> ATS410=0
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$I=10
>> AT$I=10
<< 002C2EA1[0x0d]
 - uart.sendData: response: 002C2EA1
 - wisol.getID: 002C2EA1
 - wisol_task OK, response: 002C2EA1
 - uart.sendData: AT$I=11
>> AT$I=11
<< 5BEB8C#64E869BD1[0x0d]
 - uart.sendData: response: 5BEB8C#64E869BD1
 - wisol.getPAC: 5BEB8C#64E869BD1
 - wisol_task OK, response: 5BEB8C#64E869BD1
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 28.78
tmp >> Release semaphore
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄5 >> Wait interval
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 59.23
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,0␀
 - uart.sendData: response: 1,0
 - wisol.checkPower: Continue CMD_PRESEND2
 - wisol_task OK, response: 1,0
 - uart.sendData: AT$RC
>> AT$RC
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
<< (Timeout)
␀> AT$SF=0102030405060708090a0b0c,1
<< ERR_S#X_ERR_SEND_FRAME_kAIT_TIMEOUT

***** uart.sendData: Error: Unknown response: ERR_S#X_ERR_SEND_FRAME_kAIT_TIMEOUT

 - wisol.getDownlink: ERR_S#X_ERR_SEND_FRAME_kAIT_TIMEOUT

 - wisol.getDownlink Result: ERR_S#X_ERR_SEND_FRAME_kAIT_TIMEOUT
***** Error: wisol_task Failed, response: ERR_S#X_ERR_SEND_FRAME_kAIT_TIMEOUT
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
hmd >> Wait for semaphore
␀b␂�������������␁␄% >> Got semaphore
alt >> poll_sensor
alt >> Send msg 43.26
alt >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄␏ >> Wait interval
␀> AT$GI?
<< 1,6␀
 - uart.sendData: response: 1,6
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,6
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC �@ 98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC �@ 98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC �@ 98 76 54 32 10
 - wisol.getDownlink Result: FEDC�@9876543210
 - wisol_task OK, response: FEDC�@9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄0 >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 29.59
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄U >> Wait interval
alt >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC �A 98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC �A 98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC �A 98 76 54 32 10
 - wisol.getDownlink Result: FEDC�A9876543210
 - wisol_task OK, response: FEDC�A9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 58.29
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
<< (Timeout)
␀> AT$SF=0102030405060708090a0b0c,1
<< O�␎
RX=FE DC BA 98 �6 54 32 10[0x0d]
␀
***** uart.sendData: Error: Unknown response: O�␎
RX=FE DC BA 98 �6 54 32 10

 - wisol.getDownlink: O�␎
RX=FE DC BA 98 �6 54 32 10

 - wisol.getDownlink Result: O�␎RX=FEDCBA98�6543210
***** Error: wisol_task Failed, response: O�␎RX=FEDCBA98�6543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 30.63
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA 9␜ 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA 9␜ 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA 9␜ 76 54 32 10
 - wisol.getDownlink Result: FEDCBA9␜76543210
 - wisol_task OK, response: FEDCBA9␜76543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 56.70
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE␐DC BA 98 76 54 32␐10␀
 - uart.sendData: response: OK
RX=FE␐DC BA 98 76 54 32␐10
 - wisol.getDownlink: OK
RX=FE␐DC BA 98 76 54 32␐10
 - wisol.getDownlink Result: FE␐DCBA98765432␐10
 - wisol_task OK, response: FE␐DCBA98765432␐10
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄N >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 31.75
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA␐98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA␐98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA␐98 76 54 32 10
 - wisol.getDownlink Result: FEDCBA␐9876543210
 - wisol_task OK, response: FEDCBA␐9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 55.81
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC aA 98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC aA 98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC aA 98 76 54 32 10
 - wisol.getDownlink Result: FEDCaA9876543210
 - wisol_task OK, response: FEDCaA9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 32.55
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄␔ >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE �C BA 98 76 54 32 �0␀
 - uart.sendData: response: OK
RX=FE �C BA 98 76 54 32 �0
 - wisol.getDownlink: OK
RX=FE �C BA 98 76 54 32 �0
 - wisol.getDownlink Result: FE�CBA98765432�0
 - wisol_task OK, response: FE�CBA98765432�0
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 54.71
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=�E DC BA 98 76 54 ␙2 10␀
 - uart.sendData: response: OK
RX=�E DC BA 98 76 54 ␙2 10
 - wisol.getDownlink: OK
RX=�E DC BA 98 76 54 ␙2 10
 - wisol.getDownlink Result: �EDCBA987654␙210
 - wisol_task OK, response: �EDCBA987654␙210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 33.22
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]␅RX=FE DC BA 98 7� 54 32 10␀
 - uart.sendData: response: OK␅RX=FE DC BA 98 7� 54 32 10
 - wisol.getDownlink: OK␅RX=FE DC BA 98 7� 54 32 10
 - wisol.getDownlink Result: OK␅RX=FEDCBA987�543210
 - wisol_task OK, response: OK␅RX=FEDCBA987�543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 53.98
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA 9� 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA 9� 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA 9� 76 54 32 10
 - wisol.getDownlink Result: FEDCBA9�76543210
 - wisol_task OK, response: FEDCBA9�76543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄␝ >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 33.66
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC aA 98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC aA 98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC aA 98 76 54 32 10
 - wisol.getDownlink Result: FEDCaA9876543210
 - wisol_task OK, response: FEDCaA9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 53.29
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1�4␀
 - uart.sendData: response: 1�4
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1�4
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA␐98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA␐98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA␐98 76 54 32 10
 - wisol.getDownlink Result: FEDCBA␐9876543210
 - wisol_task OK, response: FEDCBA␐9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 33.98
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA 9␜ 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA 9␜ 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA 9␜ 76 54 32 10
 - wisol.getDownlink Result: FEDCBA9␜76543210
 - wisol_task OK, response: FEDCBA9␜76543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 52.89
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX�FE DC BA 98 76 54�32 10␀
 - uart.sendData: response: OK
RX�FE DC BA 98 76 54�32 10
 - wisol.getDownlink: OK
RX�FE DC BA 98 76 54�32 10
 - wisol.getDownlink Result: OKRX�FEDCBA987654�3210
 - wisol_task OK, response: OKRX�FEDCBA987654�3210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 34.29
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄␆ >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA �8 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA �8 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA �8 76 54 32 10
 - wisol.getDownlink Result: FEDCBA�876543210
 - wisol_task OK, response: FEDCBA�876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 51.98
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC B` 98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC B` 98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC B` 98 76 54 32 10
 - wisol.getDownlink Result: FEDCB`9876543210
 - wisol_task OK, response: FEDCB`9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄␕ >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 34.58
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
<< (Timeout)
␀> AT$SF=0102030405060708090a0b0c,1
<< ERR_SFX_ERR_IEND_FRAME_WAIT_TIfEOUT

***** uart.sendData: Error: Unknown response: ERR_SFX_ERR_IEND_FRAME_WAIT_TIfEOUT

 - wisol.getDownlink: ERR_SFX_ERR_IEND_FRAME_WAIT_TIfEOUT

 - wisol.getDownlink Result: ERR_SFX_ERR_IEND_FRAME_WAIT_TIfEOUT
***** Error: wisol_task Failed, response: ERR_SFX_ERR_IEND_FRAME_WAIT_TIfEOUT
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 51.37
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,6␀
 - uart.sendData: response: 1,6
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,6
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE �A BA 98 76 54 32 80␀
 - uart.sendData: response: OK
RX=FE �A BA 98 76 54 32 80
 - wisol.getDownlink: OK
RX=FE �A BA 98 76 54 32 80
 - wisol.getDownlink Result: FE�ABA9876543280
 - wisol_task OK, response: FE�ABA9876543280
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄] >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 34.93
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄+ >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
<< (Timeout)
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC AA 98 76 54 32 10�
␀
***** uart.sendData: Error: Unknown response: OK
RX=FE DC AA 98 76 54 32 10�

 - wisol.getDownlink: OK
RX=FE DC AA 98 76 54 32 10�

 - wisol.getDownlink Result: FEDCAA9876543210�
***** Error: wisol_task Failed, response: FEDCAA9876543210�
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 50.64
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]␅RX=FE DC BA 98 76␐54 32 10␀
 - uart.sendData: response: OK␅RX=FE DC BA 98 76␐54 32 10
 - wisol.getDownlink: OK␅RX=FE DC BA 98 76␐54 32 10
 - wisol.getDownlink Result: OK␅RX=FEDCBA9876␐543210
 - wisol_task OK, response: OK␅RX=FEDCBA9876␐543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 35.14
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄n >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA�<8 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA�<8 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA�<8 76 54 32 10
 - wisol.getDownlink Result: FEDCBA�<876543210
 - wisol_task OK, response: FEDCBA�<876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 50.16
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE D! BA 98 76 54 32 1␘␀
 - uart.sendData: response: OK
RX=FE D! BA 98 76 54 32 1␘
 - wisol.getDownlink: OK
RX=FE D! BA 98 76 54 32 1␘
 - wisol.getDownlink Result: FED!BA987654321␘
 - wisol_task OK, response: FED!BA987654321␘
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄| >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 35.26
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄␂ >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC B` 98 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC B` 98 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC B` 98 76 54 32 10
 - wisol.getDownlink Result: FEDCB`9876543210
 - wisol_task OK, response: FEDCB`9876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 49.33
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
<< (Timeout)
␀> AT$SF=0102030405060708090a0b0c,1
<< OK�
RX=FE DC BA 98 7� 54 32 10[0x0d]
␀
***** uart.sendData: Error: Unknown response: OK�
RX=FE DC BA 98 7� 54 32 10

 - wisol.getDownlink: OK�
RX=FE DC BA 98 7� 54 32 10

 - wisol.getDownlink Result: OK�RX=FEDCBA987�543210
***** Error: wisol_task Failed, response: OK�RX=FEDCBA987�543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄� >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 35.37
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄q >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
Rl=FE DC BA 98 76 :4 32 10␀
 - uart.sendData: response: OK
Rl=FE DC BA 98 76 :4 32 10
 - wisol.getDownlink: OK
Rl=FE DC BA 98 76 :4 32 10
 - wisol.getDownlink Result: OKRl=FEDCBA9876:43210
 - wisol_task OK, response: OKRl=FEDCBA9876:43210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 49.10
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=FE DC BA �8 76 54 32 10␀
 - uart.sendData: response: OK
RX=FE DC BA �8 76 54 32 10
 - wisol.getDownlink: OK
RX=FE DC BA �8 76 54 32 10
 - wisol.getDownlink Result: FEDCBA�876543210
 - wisol_task OK, response: FEDCBA�876543210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄␁ >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 35.48
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX␞FE DC BA 98 76 54␐32 10␀
 - uart.sendData: response: OK
RX␞FE DC BA 98 76 54␐32 10
 - wisol.getDownlink: OK
RX␞FE DC BA 98 76 54␐32 10
 - wisol.getDownlink Result: OKRX␞FEDCBA987654␐3210
 - wisol_task OK, response: OKRX␞FEDCBA987654␐3210
 - wisol.getCmdSend
 - wisol.getCmdPower
tmp >> Wait for semaphore
␄␙ >> Got semaphore
hmd >> poll_sensor
hmd >> Send msg 49.00
hmd >> Release semaphore
 - uart.sendData: AT$GI?
␄␙ >> Wait interval
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1
␀> AT$SF=0102030405060708090a0b0c,1
<< OK[0x0d]
RX=�E DC BA 98 76 54 92 10␀
 - uart.sendData: response: OK
RX=�E DC BA 98 76 54 92 10
 - wisol.getDownlink: OK
RX=�E DC BA 98 76 54 92 10
 - wisol.getDownlink Result: �EDCBA9876549210
 - wisol_task OK, response: �EDCBA9876549210
 - wisol.getCmdSend
 - wisol.getCmdPower
␀b␂�������������␁␄␡ >> Got semaphore
tmp >> poll_sensor
tmp >> Send msg 35.56
tmp >> Release semaphore
 - uart.sendData: AT$GI?
␀b␂�������������␁␄� >> Wait interval
hmd >> Wait for semaphore
␀> AT$GI?
<< 1,5␀
 - uart.sendData: response: 1,5
 - wisol.checkPower: Change CMD_PRESEND2
 - wisol_task OK, response: 1,5
 - uart.sendData: AT
␀> AT
<< OK[0x0d]
 - uart.sendData: response: OK
 - wisol_task OK, response: OK
 - uart.sendData: AT$SF=0102030405060708090a0b0c,1

```
