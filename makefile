
mcu = stm32f4

CPP_COMPILE_PATH = /path/to/toolchain/bin/arm-none-eabi-g++
C_COMPILE_PATH =   /path/to/toolchain/bin/arm-none-eabi-gcc
ASSEM_PATH =       /path/to/toolchain/bin/arm-none-eabi-gcc
LINKER_PATH =      /path/to/toolchain/bin/arm-none-eabi-g++

CPP_FLAGS = -c -mcpu=cortex-m4 -mthumb -mlittle-endian -O2 -fmessage-length=0 -fsigned-char -ffunction-sections -fdata-sections -fno-rtti -fno-exceptions -fno-threadsafe-statics -g3 -ggdb -std=gnu++11 -fabi-version=0 -MMD -MP
C_FLAGS = -c -mcpu=cortex-m4 -mthumb -mlittle-endian -O2 -fmessage-length=0 -fsigned-char -ffunction-sections -fdata-sections  -g3 -ggdb -std=gnu11 -nostdlib -MMD -MP
ASSEM_FLAGS = -mcpu=cortex-m4 -mthumb -mlittle-endian -O2 -fmessage-length=0 -fsigned-char -ffunction-sections -fdata-sections  -g3 -ggdb -x assembler-with-cpp
LINKER_FLAGS = -mcpu=cortex-m4 -mthumb -mlittle-endian -O2 -fmessage-length=0 -fsigned-char -ffunction-sections -fdata-sections  -g3 -ggdb -T "./stm32f4/STM32F417IG_FLASH.ld" -Xlinker --gc-sections -Wl,-Map,"app.map"  -o "app.elf"

link = $(LINKER_PATH) $(LINKER_FLAGS)
COMPILE_C = $(C_COMPILE_PATH) $(C_FLAGS)
COMPILE_CPP = $(CPP_COMPILE_PATH) $(CPP_FLAGS)
ASSEM = $(ASSEM_PATH) $(ASSEM_FLAGS)

includes = -I. \
-I./stm32/porting \
-I./stm32f4/inc \
-I./radios \
-I./sensors \
-I../cocoOS/common/inc \
-I../stmlib/stm32f4 \
-I../stmlib/stm32f4/inc

source = . ./stm32f4/src ./radios ./sensors
output = ./output
modpath = ..


# for each source path above, collect all .cpp, .c and .s files. These are the application sources.
sources = $(foreach path, $(source), $(notdir $(wildcard $(path)/*.cpp))) $(foreach path, $(source), $(notdir $(wildcard $(path)/*.c))) $(foreach path, $(source), $(notdir $(wildcard $(path)/*.S)))

# from the sources, define the application object files.
appobjs = $(patsubst %,$(output)/%,$(addsuffix .o,$(basename $(sources))))

# list of all modules that the application depends on
moddeps = stmlib cocoOS

# for each module, define the module object files. 
moduleobjs = $(foreach module, $(moddeps), $(wildcard $(modpath)/$(module)/$(mcu)/obj/*.o) ) $(foreach module, $(moddeps), $(wildcard $(modpath)/$(module)/common/obj/*.o) )
	

prog: $(moddeps) application
	$(link) $(moduleobjs) $(appobjs)
	
	
application: $(appobjs)


$(moddeps) : 
	cd $(modpath)/$@ && $(MAKE) mcu=$(mcu)

./output/%.o : %.cpp
	$(COMPILE_CPP) $(includes) -DSTM32F4 -DDISABLE_DEBUG_LOG -c $^ -o $@
	
./output/%.o : ./radios/%.cpp
	$(COMPILE_CPP) $(includes) -DSTM32F4 -DDISABLE_DEBUG_LOG -c $^ -o $@

./output/%.o : ./sensors/%.cpp
	$(COMPILE_CPP) $(includes) -DSTM32F4 -DDISABLE_DEBUG_LOG -c $^ -o $@
	
./output/%.o : ./$(mcu)/src/%.cpp
	$(COMPILE_CPP) $(includes) -c $^ -o $@

./output/%.o : %.c
	$(COMPILE_C) $(includes) -c $^ -o $@

./output/%.o : ./$(mcu)/src/%.c
	$(COMPILE_C) $(includes) -c $^ -o $@
	
./output/%.o : %.S
	$(ASSEM) $(includes) -c $^ -o $@

./output/%.o : ./$(mcu)/src/%.S
	$(ASSEM) $(includes) -c $^ -o $@
	
names:
	@echo $(moduleobjs)
	
clean:
	rm ./output/*
	

	
