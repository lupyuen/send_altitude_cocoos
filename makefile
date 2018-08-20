include ../flags.mk

includes = -I. -I./$(mcu)/inc \
-I../mcal/$(mcu) \
-I../cocoOS/common/inc \
-I../utils/common \
-I../stmlib/$(mcu) \
-I../stmlib/$(mcu)/inc

source = . ./$(mcu)/src
output = ./output
modpath = ..


# for each source path above, collect all .cpp, .c and .s files. These are the application sources.
sources = $(foreach path, $(source), $(notdir $(wildcard $(path)/*.cpp))) $(foreach path, $(source), $(notdir $(wildcard $(path)/*.c))) $(foreach path, $(source), $(notdir $(wildcard $(path)/*.S)))

# from the sources, define the application object files.
appobjs = $(patsubst %,$(output)/%,$(addsuffix .o,$(basename $(sources))))

# list of all modules that the application depends on
moddeps = stmlib mcal utils cocoOS

# for each module, define the module object files. 
moduleobjs = $(foreach module, $(moddeps), $(wildcard $(modpath)/$(module)/$(mcu)/obj/*.o) ) $(foreach module, $(moddeps), $(wildcard $(modpath)/$(module)/common/obj/*.o) )


name: 
	@echo $(sources)
	
#prog: application modules
prog: $(moddeps) application
	$(link) $(moduleobjs) $(appobjs)
	
	
application: $(appobjs)

#modules: 
#	cd ../../modules/mcal && $(MAKE) mcu=stm32f4

$(moddeps) : 
	cd $(modpath)/$@ && $(MAKE)

./output/%.o : %.cpp
	$(COMPILE_CPP) $(includes) -DSTM32F4 -c $^ -o $@
	

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
	
help:
	@echo 
	@echo Usage: make [target] mcu=[mcu] host=[host]
	@echo
	@echo Builds and links executable on the specified host for the specified mcu
	@echo 
	@echo "[target]:	application, builds only application. No linking."
	@echo "		prog, builds application and modules. Link executable."
	@echo 
	@echo "[mcu]:		stm32f3"
	@echo "		stm32f4"
	@echo  
	@echo "[host]:		win"
	@echo "		linux"
	
