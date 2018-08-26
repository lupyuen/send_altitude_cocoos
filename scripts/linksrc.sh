# Set up the symbolic links for source files and for cocoOS config file.

# To compile the project under PlatformIO in Visual Studio Code, 
# the source files should be symbolically linked into the folder "src"
mkdir lib
ln -s lib libraries
mkdir src
cd src
ln -s ../*.ino ../*.cpp ../*.h .
cd ..

# Remove the default cocoOS config and link to our version.
cd lib/cocoOS_*/src
rm os_defines.h
ln -s ../../../os_defines.h .
cd ../../..

# Launch Visual Studio Code.  Sometimes the Code Helper is hung.  Kill it.
pkill "Visual Studio Code"
pkill "Code Helper"
open -a "Visual Studio Code"
