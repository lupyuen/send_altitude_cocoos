::  Download the Boost libraries into the lib folder and create the "src" links.

::  This is the version of Boost we will download.  Must sync with install_boost.sh.
set boost_version=boost-1.68.0

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
goto :EOF

::  Download the Boost library named by $1.
:download_boost %1
if not exist lib mkdir lib
if not exist lib\boost_%1 mkdir lib\boost_%1
cd lib\boost_%1

:: Erase the existing Boost library and the "boost" link just in case.
if exist %1 rd /s /q %1
if exist src\boost del src\boost

::  Download Boost library from GitHub.
if not exist %1 git clone https://github.com/boostorg/%1.git --branch %boost_version --single-branch
if not exist src mkdir src
cd src

::  The boost_xxx.hpp file should come from our GitHub repo.  If missing, create it.
if not exist boost_%1.hpp echo //  Force boost_%1 library to be included. >boost_%1.hpp
if not exist boost mklink /d boost ..\%1\include\boost
cd ..\..\..
goto :EOF
