cd lib
mkdir bluepill-micropython
cd bluepill-micropython
mkdir src
cd src

set mpy=..\..\..\..\bluepill-micropython
set dir=py

mkdir %dir%
FOR %%f IN (%mpy%\%dir%\*.c) DO mklink %%~nf%%~xf %mpy%\%dir%\%%~nf%%~xf
FOR %%f IN (%mpy%\%dir%\*.h) DO mklink %dir%\%%~nf%%~xf ..\%mpy%\%dir%\%%~nf%%~xf

cd ..\..\..
