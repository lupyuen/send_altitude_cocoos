:: Set up the symbolic links for source files and for cocoOS config file.

:: To compile the project under PlatformIO in Visual Studio Code, 
:: the source files should be symbolically linked into the folder "src"
mkdir lib
mklink libraries lib
mkdir src
cd src
FOR %f IN (..\*.ino ..\*.cpp ..\*.h) DO mklink %~nf%~xf ..\%~nf%~xf
cd ..

:: Remove the default cocoOS config and link to our version.
cd lib\cocoOS_*\src
del os_defines.h
mklink os_defines.h ..\..\..\os_defines.h
cd ..\..\..
