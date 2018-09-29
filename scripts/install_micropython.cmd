::  TODO: Location of bluepill-micropython repo.
set mpy=%CD%\..\bluepill-micropython

::  Location of src folder.
set src=%CD%\lib\bluepill-micropython\src

::  Erase the current links in src.
rd /s /q %src%
mkdir %src%
cd %src%

::  Link the ports\bluepill files into lib\bluepill-micropython\src.
set src_ports=%src%\..\ports\bluepill
cd %src%
echo %CD%
FOR %%f IN (%src_ports%\*.*) DO mklink %%~nf%%~xf %src_ports%\%%~nf%%~xf

::  Handle each folder.
call :link_folder py
call :link_folder extmod
call :link_folder drivers\bus

::  Done
cd %src%
cd ..\..\..
goto :EOF


::  %1 is the name of the folder in bluepill-micropython, e.g. "py".
:link_folder %1
set dir=%1
set src_dir=%src%\%dir%
set mpy_dir=%mpy%\%dir%

::  Link the bluepill-micropython\???\*.c files into lib\bluepill-micropython\src.
cd %src%
echo %CD%
echo "ln -s %mpy_dir%\*.c ."
FOR %%f IN (%mpy_dir%\*.c) DO mklink %%~nf%%~xf %mpy_dir%\%%~nf%%~xf

::  Link the bluepill-micropython\???\*.h files into lib\bluepill-micropython\src\???.
mkdir %src_dir%
cd %src_dir%
echo %CD%
echo "ln -s %mpy_dir%\*.h ."
FOR %%f IN (%mpy_dir%\*.h) DO mklink %%~nf%%~xf %mpy_dir%\%%~nf%%~xf

::  Return to caller.
goto :EOF
