# Set up the symbolic links for source files and for cocoOS config file.

# To compile the project under PlatformIO in Visual Studio Code, 
# the source files should be symbolically linked into the folder "src"
mkdir lib
mkdir src
cd src
ln -s ../*.ino ../*.cpp ../*.h .
cd ..

# Remove the default cocoOS config and link to our version.
cd lib/cocoOS_*/src
rm os_defines.h
ln -s ../../../os_defines.h .
cd ../../..

# Launch our Visual Studio Code workspace.  Sometimes the Code Helper is hung, kill it.
pkill "Visual Studio Code"
pkill "Code Helper"
open workspace.code-workspace
