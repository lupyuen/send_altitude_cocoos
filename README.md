IoT program for Arduino Uno and STM32 Blue Pill (with libopencm3) to read BME280 I2C temperature, humidity and altitude sensors and send the aggregated sensor data to the Sigfox IoT network (via the Wisol Sigfox module connected on UART).  Supports Sigfox downlink too. Multitasking of the sensors is handled by cocoOS, the portable task scheduling framework: http://www.cocoos.net

Read the tutorials:

- Part 1: _Juggling Arduino Sensors With cocoOS_<br>
    https://medium.com/coinmonks/juggling-arduino-sensors-with-cocoos-403e14ec28be

- Part 2: _Juggling Sigfox Downlink And Arduino Sensors With cocoOS_<br>
    https://medium.com/coinmonks/juggling-sigfox-downlink-and-arduino-sensors-with-cocoos-4594be59bf1b

- Part 3: _Juggling STM32 Blue Pill For Arduino Jugglers_<br>
    https://medium.com/@ly.lee/juggling-stm32-blue-pill-for-arduino-jugglers-edf6820dc808

- Part 4: _Watch STM32 Blue Pill Juggle Two SPI Sensors With DMA_<br>
    https://medium.com/@ly.lee/watch-stm32-blue-pill-juggle-two-spi-sensors-with-dma-20cd1aa89869

The code compiles under the Arduino IDE, Arduino Web Editor, and Visual Studio Code with the PlatformIO extension installed. You'll need to install the following libraries:

- `cocoOS_5.0.3`: Download from http://www.cocoos.net/download.html. <br>
    For Arduino IDE and Arduino Web Editor: <br>
    Unzip and move all files in inc and src to top level. <br>
    Replace os_defines.h by our custom cocoOS settings: <br>
    https://github.com/lupyuen/send_altitude_cocoos/blob/master/os_defines.h <br>
    Zip up and add to Arduino IDE or Arduino Web Editor as a library.

- `BME280` (for Arduino IDE and Arduino Web Editor only): Download the `BME280` library by Tyler Glenn from Arduino Library Manager

Tested with Arduino Uno and STM32 Blue Pill.

-----
## Select Features By Article

Each tutorial article in the series is configured with a different set of features.
To allow new articles to be published without breaking the features of the older articles,
we configure the article that we wish to use in `platform.h`

Edit `platform.h` and uncomment only ONE of these lines:

```c++
//  Configure the features according to the article.  Only one of these CONFIG_ARTICLEx should be defined.
//  #define CONFIG_ARTICLE1  //  Uncomment to support Article #1: 
                             //  "Juggling Arduino Sensors With cocoOS" 
//  #define CONFIG_ARTICLE2  //  Uncomment to support Article #2: 
                             //  "Juggling Sigfox Downlink And Arduino Sensors With cocoOS" 
//  #define CONFIG_ARTICLE3  //  Uncomment to support Article #3: 
                             //  "Juggling STM32 Blue Pill For Arduino Jugglers"
//  #define CONFIG_ARTICLE4  //  Uncomment to support Article #4: 
                             //  "Watch STM32 Blue Pill Juggle Two SPI Sensors With DMA"
//  (...plus future articles)
```

-----
## Create Source File Links For PlatformIO

To compile the project under PlatformIO in Visual Studio Code, the source files
should be symbolically linked into the folder `src`

Open a Windows, Mac or Ubuntu command prompt and enter:

```bash
git clone https://github.com/lupyuen/send_altitude_cocoos.git
cd send_altitude_cocoos
```

Download cocoOS_5.0.3 from http://www.cocoos.net/download.html

Copy the cocoOS_5.0.3 source files (`inc/*.h, src/*.c`) into send_altitude_cocoos at this subfolder:

`lib/cocoOS_5.0.3/src`

There should not be any folders inside `lib/cocoOS_5.0.3/src`

Then enter the following into the command prompt…

_For Windows:_

```cmd
scripts\linksrc.cmd
```

_For Mac and Ubuntu:_

```bash
chmod +x scripts/*.sh
scripts/linksrc.sh
```

To allow Arduino IDE and PlatformIO to share the same cocoOS library `cocoOS_5.0.3`:

```bash
ln -s ~/send_altitude_cocoos/lib/cocoOS_5.0.3/src/ ~/Documents/Arduino/libraries/cocoOS_5.0.3
```

Arduino IDE does not compile if `send_altitude_cocoos/src` contains any files. Run `scripts/unlinksrc.sh` to remove the links before compiling in Arduino IDE.

-----
## Build for STM32 Blue Pill with PlatformIO

To build for STM32 Blue Pill on Visual Studio Code and PlatformIO, edit `platformio.ini` and uncomment the `bluepill_f103c8` line (by removing `;` in front):

```ini
env_default = bluepill_f103c8
```

And comment the `uno` line (by inserting `;` in front):

```ini
; env_default = uno
```

### Install OpenOCD For Displaying STM32 Blue Pill Debug Log

- For Windows:

  1. Download OpenOCD (for debugging the Blue Pill) from the unofficial OpenOCD release website: <br>
    https://github.com/gnu-mcu-eclipse/openocd/releases <br>
    Look for `gnu-mcu-eclipse-openocd-…-win64.zip`

  1. Unzip the OpenOCD download and copy the OpenOCD files into `c:\openocd` such that `opencd.exe` is located in the folder `c:\openocd\bin`

- For Mac:

  ```bash
  brew install openocd
  ```

- For Ubuntu:

  ```bash
  sudo apt install openocd
  ```

### For Windows only: Install ST-Link USB Driver

1. For Windows only: Download the ST-Link USB driver from the ST-Link Driver Website (email registration required): <br>
  http://www.st.com/en/embedded-software/stsw-link009.html

1. Scroll down and click the `Get Software` button

1. Unzip the ST-Link download. Double-click the `dpinst_amd64.exe` installer.

-----
## Enabling I2C Interface on STM32 Blue Pill

To enable the actual STM32 Blue Pill I2C interface to read a real BME280 sensor module (instead of using the BME280 I2C simulator), edit `stm32/i2cint/i2cint.cpp`
and comment the following line:

```C++
//  #define SIMULATE_BME280  //  Uncomment to simulate a BME280 sensor connected to I2C Bus.
```

Rebuild the project. We support only Blue Pill I2C Port 1: <br>
`SCL1` = Pin `PB6` <br>
`SDA1` = Pin `PB7`

-----
## Enabling UART Interface on STM32 Blue Pill

To enable the actual STM32 Blue Pill UART interface to transmit via a real Wisol Sigfox transceiver (instead of using the Wisol Sigfox simulator), edit `stm32/uartint/uartint.cpp`
and comment the following line:

```C++
//  #define SIMULATE_WISOL //  Uncomment to simulate a Wisol Sigfox module connected to UART.
```

Then install the Boost libraries by executing the following script:

_For Windows:_

```cmd
cd send_altitude_cocoos
scripts\install_boost.cmd
```

_For Mac and Ubuntu:_

```bash
cd send_altitude_cocoos
scripts/install_boost.sh
```

Rebuild the project. We support only Blue Pill USART Port 2: <br>
`RX2` = Pin `PA3` <br>
`TX2` = Pin `PA2`

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

[`display.cpp`](display.cpp), [`display.h`](display.h): Display Task to display sensor data

[`scripts/connect.ocd`](scripts/connect.ocd): OpenOCD script to connect to Blue Pill via ST-Link debugger, restart the Blue Pill and display the debug log (Arm Semihosting).  Called by the `Connect To STM32 Blue Pill` task.

-----
### STM32 Code

[`stm32`](stm32): Platform-specific code for STM32 Blue Pill

[`stm32/bluepill`](stm32/bluepill): STM32 Blue Pill common functions

[`stm32/logger`](stm32/logger): Debugger logging library based on ARM Semihosting

[`stm32/i2cint`](stm32/i2cint): I2C Interface. Simulates BME280 sensor.

[`stm32/uartint`](stm32/uartint): UART Interface. Simulates Wisol Sigfox transceiver.

[`stm32/porting`](stm32/porting): Library for porting Arduino code to STM32

-----
### Build and Configuration Files

[`.pioenvs`](.pioenvs): Build files for PlatformIO

[`.travis.yml`](.travis.yml): Travis CI file (outdated)

[`.vscode`](.vscode): Configuration files for Visual Studio Code

[`.vscode/tasks.json`](.vscode/tasks.json): Defines the `Connect To STM32 Blue Pill` task for displaying the Blue Pill log via OpenOCD

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
<< 5BEB8CF64E869BD1[0x0d]
 - wisol.getPAC: 5BEB8CF64E869BD1
net >> Release net
tmp >> poll_sensor
hmd >> poll_sensor
alt >> poll_sensor
alt << Recv sensor data 133.44
tmp >> poll_sensor
tmp << Recv sensor data 34.20
hmd >> poll_sensor
hmd << Recv sensor data 54.59
alt >> poll_sensor
alt << Recv sensor data 133.44
tmp >> poll_sensor
tmp << Recv sensor data 34.19
hmd >> poll_sensor
hmd << Recv sensor data 54.59
alt >> poll_sensor
alt << Recv sensor data 133.09
tmp >> poll_sensor
tmp << Recv sensor data 34.19
hmd >> poll_sensor
hmd << Recv sensor data 54.59
alt >> poll_sensor
alt << Recv sensor data 133.09
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.59
alt >> poll_sensor
alt << Recv sensor data 135.23
agg >> Send 0000034105451352
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,6[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0000034105451352,1[0x0d]
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.60
alt >> poll_sensor
alt << Recv sensor data 134.17
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.61
alt >> poll_sensor
alt << Recv sensor data 134.88
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.63
alt >> poll_sensor
alt << Recv sensor data 133.46
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.63
alt >> poll_sensor
alt << Recv sensor data 133.82
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.63
alt >> poll_sensor
alt << Recv sensor data 132.99
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.62
alt >> poll_sensor
alt << Recv sensor data 134.05
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.62
alt >> poll_sensor
alt << Recv sensor data 132.05
<< OK[0x0d][0x0a]RX=FE DC BA 98 76 54 32 10[0x0d]
net >> Pending response
 - process_downlink_msg: FEDCBA9876543210
tmp >> poll_sensor
tmp << Recv sensor data 34.16
hmd >> poll_sensor
hmd << Recv sensor data 54.65
alt >> poll_sensor
alt << Recv sensor data 132.29
tmp >> poll_sensor
tmp << Recv sensor data 34.16
hmd >> poll_sensor
hmd << Recv sensor data 54.62
alt >> poll_sensor
alt << Recv sensor data 132.40
tmp >> poll_sensor
tmp << Recv sensor data 34.16
hmd >> poll_sensor
hmd << Recv sensor data 54.62
alt >> poll_sensor
alt << Recv sensor data 133.58
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.62
alt >> poll_sensor
alt << Recv sensor data 133.23
tmp >> poll_sensor
tmp << Recv sensor data 34.17
agg >> Send 0001034105461332
net >> Wait for net
net >> Got net
>> AT$GI?[0x0d]
<< 1,5[0x0d]
>> AT[0x0d]
<< OK[0x0d]
net >> Release net
>> AT$SF=0001034105461332,1[0x0d]
hmd >> poll_sensor
hmd << Recv sensor data 54.62
alt >> poll_sensor
alt << Recv sensor data 132.05
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.64
alt >> poll_sensor
alt << Recv sensor data 133.95
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.63
alt >> poll_sensor
alt << Recv sensor data 133.46
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.63
alt >> poll_sensor
alt << Recv sensor data 133.23
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.65
alt >> poll_sensor
alt << Recv sensor data 132.88
tmp >> poll_sensor
tmp << Recv sensor data 34.16
hmd >> poll_sensor
hmd << Recv sensor data 54.64
alt >> poll_sensor
alt << Recv sensor data 133.95
tmp >> poll_sensor
tmp << Recv sensor data 34.15
hmd >> poll_sensor
hmd << Recv sensor data 54.63
alt >> poll_sensor
alt << Recv sensor data 133.71
tmp >> poll_sensor
tmp << Recv sensor data 34.16
hmd >> poll_sensor
hmd << Recv sensor data 54.65
alt >> poll_sensor
alt << Recv sensor data 134.54
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.68
alt >> poll_sensor
alt << Recv sensor data 133.12
tmp >> poll_sensor
tmp << Recv sensor data 34.16
hmd >> poll_sensor
hmd << Recv sensor data 54.66
alt >> poll_sensor
alt << Recv sensor data 132.53
tmp >> poll_sensor
tmp << Recv sensor data 34.17
hmd >> poll_sensor
hmd << Recv sensor data 54.67
alt >> poll_sensor
alt << Recv sensor data 134.88
tmp >> poll_sensor
tmp << Recv sensor data 34.16
hmd >> poll_sensor
hmd << Recv sensor data 54.66
alt >> poll_sensor
alt << Recv sensor data 133.46
tmp >> poll_sensor
tmp << Recv sensor data 34.16
<< (Timeout)
<< ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT[0x0d][0x0a]
***** uart.sendData: Error: Unknown response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT

net >> Pending response
***** Error: network_task Failed, response: ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
 - process_downlink_msg (Failed): ERR_SFX_ERR_SEND_FRAME_WAIT_TIMEOUT
hmd >> poll_sensor
hmd << Recv sensor data 54.64
alt >> poll_sensor
alt << Recv sensor data 133.82
tmp >> poll_sensor
tmp << Recv sensor data 34.18
hmd >> poll_sensor
hmd << Recv sensor data 54.63
alt >> poll_sensor
alt << Recv sensor data 133.34


```
