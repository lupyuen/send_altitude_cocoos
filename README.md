A demonstration of the IoT platform send_altitude_cocoos ported to the ST DiscoveryF4 board. Position aquired from a GPS module is sent to Sigfox network.

# Introduction
This project is based on the send_altitude_cocoos platform and follows its pattern: sensors are periodically polled for new readings that are sent to an aggregator. The aggregator in turn is setup to periodically send the collected readings to the network task for transmission. Finally the network task sends the sensor readings as a package to the connected Sigfox radio.

### Repo structure changes
However, the code has been refactored and restructured. Some files have been removed, and others have been renamed. All references to Arduino are removed, so is also all the debug prints to make the code cleaner and more readable. File tree was restructured to separate platform files at top level from project specific files in subfolders:
```
send_altitude_cocoos
|--platform files
|--main.cpp
|--radios
|  |--wisolRadio.cpp
|  |--wisolRadio.h
|--sensors
|  |--gps_sensor.cpp
|  |--gps_sensor.h
|--stm32f4
```

### Software Design
The design of the system is shown below:
![GitHub Logo](https://github.com/lupyuen/send_altitude_cocoos/blob/discoveryF4/send_altitude_cocoos.png)
The cyan boxes is part of the platform and should (ideally) never be touched.
The pink boxes is custom parts of the system: sensors, radio transceivers and serial connections. This is where you add stuff.
  
## Hardware and required software:

 - The code in this repo
 - cocoOS, v5.0.3: https://github.com/cocoOS/cocoOS should be located at same level as send_altitude_cocoos
 - A folder named stmlib at same level as send_altitude_cocoos, with inc and src subfolders containing the ST firmware lib files.
 - STM32F4DISCOVERY Discovery kit with MCU STM32F407VG
 - Sigfox breakout board BRKWS01 based on Wisol SFM10R1: https://yadom.eu/carte-breakout-sfm10r1.html
 - Adafruit Ultimate GPS breakout based on the MTK3339 chipset: https://www.adafruit.com/product/746
 
## Build and deploy for Linux
The project uses gcc-arm-none-eabi toolchain for compiling and linking:
 - https://developer.arm.com/open-source/gnu-toolchain/gnu-rm/downloads. Untar the file in a suitable directory, e.g. ~/opt/toolchains/gcc-arm-none-eabi
 - Enter the path to the installation folder in the send_altitude_cocoos/makefile.
 - Cd to directory with send_altitude_cocoos repo and enter `make prog mcu=stm32f4 host=linux` to compile and link the application.
 
 To download the application binary to the discovery board we use st-link. 
  - `sudo apt-get install libusb-1.0-0-dev`
  - In a suitable directory: `git clone https://github.com/texane/stlink stlink.git`
   - Then:
  ```cd stlink.git
make release
make debug
cd build/Release; make install DESTDIR=$HOME
```
Now stlink is installed under `~/usr/local.`
Verify by
```
lsusb
Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 002 Device 005: ID 0e0f:0008 VMware, Inc. 
Bus 002 Device 004: ID 0483:3748 STMicroelectronics ST-LINK/V2           <------------- Yes ! 
Bus 002 Device 003: ID 0e0f:0002 VMware, Inc. Virtual USB Hub
Bus 002 Device 002: ID 0e0f:0003 VMware, Inc. Virtual Mouse
Bus 002 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
```

Setup udev rules: `sudo nano /etc/udev/rules.d/49-stlinkv2-1.rules`
Enter following lines:
```
SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3748", \
MODE:="0666", \
SYMLINK+="stlinkv2-1_%n"
```
Make sure the numbers for idVendor and idProduct matches what was listed when you entered lsusb above.

Now we are almost done!

 - Set environment variable LD_LIBRARY_PATH to the st-link lib folder: `export LD_LIBRARY_PATH=$HOME/usr/local/lib`
 - cd to the send_altitude_cocoos directory. It should contain the binary app.elf, created in the build step above. Create a .bin file: `/path/to/toolchain/arm-none-eabi-objcopy -O binary app.elf app.bin`
 - Flash it: `~/usr/local/bin/st-flash write app.bin 0x08000000` 

 More contents will follow here....
