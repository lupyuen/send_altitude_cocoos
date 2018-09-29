::  TODO: Location of bluepill-micropython repo.
set mpy=%CD%\..\bluepill-micropython

::  Location of src folder.
set src=%CD%\lib\bluepill-micropython\src

::  Erase the current links in src.
::::rd /s %src%
mkdir %src%
cd %src%

::  Link the ports\bluepill files into lib\bluepill-micropython\src.
cd %src%
echo %CD%
echo "ln -s ..\ports\bluepill\* ."
:: ln -s ..\ports\bluepill\* .

::::FOR %%f IN (%mpy%\%dir%\*.c) DO mklink %%~nf%%~xf %mpy%\%dir%\%%~nf%%~xf

::  Handle each folder.
call :link_folder py
call :link_folder extmod
call :link_folder drivers\bus

::  Done
goto :EOF


::  %1 is the name of the folder in bluepill-micropython, e.g. "py".
:link_folder %1
dir=%1
src_dir=%src%\%dir%
mpy_dir=%mpy%\%dir%

::  Link the bluepill-micropython\???\*.c files into lib\bluepill-micropython\src.
cd %src%
echo %CD%
echo "ln -s %mpy_dir%\*.c ."
:: ls -l %mpy_dir%\*.c
:: ln -s %mpy_dir%\*.c .

::::FOR %%f IN (%mpy%\%dir%\*.c) DO mklink %%~nf%%~xf %mpy%\%dir%\%%~nf%%~xf

::  Link the bluepill-micropython\???\*.h files into lib\bluepill-micropython\src\???.
mkdir %src_dir%
cd %src_dir%
echo %CD%
echo "ln -s %mpy_dir%\*.h ."
:: ls -l %mpy_dir%\*.h .
:: ln -s %mpy_dir%\*.h .

::::FOR %%f IN (%mpy%\%dir%\*.h) DO mklink %dir%\%%~nf%%~xf ..\%mpy%\%dir%\%%~nf%%~xf

::  Return to caller.
goto :EOF
