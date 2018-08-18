# To compile the project under PlatformIO in Visual Studio Code, 
# the source files should be symbolically linked into the folder `src`

cd src
ln -s ../*.ino ../*.cpp ../*.h .
cd ..

# Launch Visual Studio Code.  Sometimes the Code Helper is hung.  Kill it.
pkill "Code Helper"
open -a "Visual Studio Code"
