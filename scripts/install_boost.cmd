goto install

:install
call :download_boost predef
goto :EOF

call :download_boost assert
call :download_boost config
call :download_boost core
call :download_boost detail
call :download_boost iterator
call :download_boost lockfree
call :download_boost mpl
call :download_boost parameter
call :download_boost predef
call :download_boost preprocessor
call :download_boost static_assert
call :download_boost type_traits
call :download_boost utility

pause

:download_boost %1
mkdir lib\boost_%1
cd lib\boost_%1
git clone https://github.com/boostorg/%1.git
mkdir src
cd src
echo //  Force boost_%1 library to be included. >boost_%1.hpp
mklink /d boost ..\%1\include\boost
cd ..\..\..
goto :EOF
