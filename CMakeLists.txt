cmake_minimum_required(VERSION 2.8.4)
set(CMAKE_VERBOSE_MAKEFILE TRUE)
set(CMAKE_TOOLCHAIN_FILE ${CMAKE_SOURCE_DIR}/cmake/ArduinoToolchain.cmake)
set(PROJECT_NAME send_altitude_cocoos)
set(PROJECT_LIB cocoos)
project(${PROJECT_NAME})
add_definitions(-DCLION)
message("CMAKE_SYSTEM_NAME: ${CMAKE_SYSTEM_NAME}")

include_directories(
        lib/cocoOS_5.0.1/src
        lib/BME280/src
)

# Arduino Options - See https://github.com/queezythegreat/arduino-cmake
# Change this to the Arduino port.
# if(WIN32)
# set(${PROJECT_NAME}_PORT COM4)
# else()
set(${PROJECT_NAME}_PORT /dev/tty.usbmodem1411)
# endif()

# Build the BME280 library.
set(BME280_SRCS
        lib/BME280/src/BME280.cpp
        lib/BME280/src/BME280I2C.cpp
        lib/BME280/src/BME280I2C_BRZO.cpp
        lib/BME280/src/BME280SpiSw.cpp
        lib/BME280/src/EnvironmentCalculations.cpp
)
set(BME280_HDRS
        lib/BME280/src/BME280.h
        lib/BME280/src/BME280I2C.h
        lib/BME280/src/BME280I2C_BRZO.h
        lib/BME280/src/BME280Spi.h
        lib/BME280/src/BME280SpiSw.h
        lib/BME280/src/EnvironmentCalculations.h
)
generate_arduino_library(BME280)

# Build the library.
set(${PROJECT_LIB}_SRCS
        lib/cocoOS_5.0.1/src/os_assert.c
        lib/cocoOS_5.0.1/src/os_cbk.c
        lib/cocoOS_5.0.1/src/os_event.c
        lib/cocoOS_5.0.1/src/os_kernel.c
        lib/cocoOS_5.0.1/src/os_msgqueue.c
        lib/cocoOS_5.0.1/src/os_sem.c
        lib/cocoOS_5.0.1/src/os_task.c
)
set(${PROJECT_LIB}_HDRS
        lib/cocoOS_5.0.1/src/cocoos.h
        lib/cocoOS_5.0.1/src/os_applAPI.h
        lib/cocoOS_5.0.1/src/os_assert.h
        lib/cocoOS_5.0.1/src/os_defines.h
        lib/cocoOS_5.0.1/src/os_event.h
        lib/cocoOS_5.0.1/src/os_msgqueue.h
        lib/cocoOS_5.0.1/src/os_port.h
        lib/cocoOS_5.0.1/src/os_sem.h
        lib/cocoOS_5.0.1/src/os_task.h
        lib/cocoOS_5.0.1/src/os_typedef.h
)
generate_arduino_library(${PROJECT_LIB})

# Build the application.
set(${PROJECT_NAME}_SRCS
        send_altitude_cocoos.ino
        aggregate.cpp
        alt_sensor.cpp
        bme280.cpp
        display.cpp
        downlink.cpp
        gyro_sensor.cpp
        humid_sensor.cpp
        main.cpp
        sensor.cpp
        temp_sensor.cpp
        uart.cpp
        wisol.cpp
)

set(${PROJECT_NAME}_HDRS
        aggregate.h
        alt_sensor.h
        bme280.h
        cocoos_cpp.h
        display.h
        downlink.h
        gyro_sensor.h
        humid_sensor.h
        platform.h
        sensor.h
        sigfox.h
        temp_sensor.h
        uart.h
        wisol.h
)

# Arduino libraries used.
set(${PROJECT_NAME}_ARDLIBS
        SoftwareSerial
)

# Custom libraries used.
set(${PROJECT_NAME}_LIBS
        ${PROJECT_LIB}
        BME280
)

generate_arduino_firmware(${PROJECT_NAME})
