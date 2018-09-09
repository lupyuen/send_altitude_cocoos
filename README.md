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
  
# Hardware and required software:

 - The code in this repo
 - cocoOS, v5.0.3: https://github.com/cocoOS/cocoOS should be located as same level as send_altitude_cocoos
 - A folder named stmlib at same level as send_altitude_cocoos, with inc and src subfolders containing the ST firmware lib files.
 - STM32F4DISCOVERY Discovery kit with MCU STM32F407VG
 - Sigfox breakout board BRKWS01 based on Wisol SFM10R1: https://yadom.eu/carte-breakout-sfm10r1.html
 - Adafruit Ultimate GPS breakout based on the MTK3339 chipset: https://www.adafruit.com/product/746
 
 # Toolchain
 The project uses gcc-arm-none-eabi toolchain for compiling and linking.
 
 More contents will follow here....
