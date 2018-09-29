#!/usr/bin/env bash

##  Location of bluepill-micropython repo.
mpy=../../../../bluepill-micropython

mkdir lib
cd lib
mkdir bluepill-micropython
cd bluepill-micropython

##  Erase the current links in src.
rm -rf src
mkdir src
cd src

##  Link the ports/bluepill files into lib/bluepill-micropython/src.
pwd
echo "ln -s ../ports/bluepill/* ."
ln -s ../ports/bluepill/* .

##  Handle folder py.
dir=py

##  Link the bluepill-micropython/py/*.c files into lib/bluepill-micropython/src.
pwd
echo "ln -s $mpy/$dir/*.c ."
ln -s $mpy/$dir/*.c .

##  Link the bluepill-micropython/py/*.h files into lib/bluepill-micropython/src/py.
mkdir $dir
cd $dir
pwd
echo "ln -s ../$mpy/$dir/*.h ."
ln -s ../$mpy/$dir/*.h .
cd ..
pwd

##  Handle folder extmode.
dir=extmod

##  Link the bluepill-micropython/extmod/*.c files into lib/bluepill-micropython/src.
pwd
echo "ln -s $mpy/$dir/*.c ."
ln -s $mpy/$dir/*.c .

##  Link the bluepill-micropython/extmod/*.h files into lib/bluepill-micropython/src/extmod.
mkdir $dir
cd $dir
pwd
echo "ln -s ../$mpy/$dir/*.h ."
ln -s ../$mpy/$dir/*.h .
cd ..
pwd

##  Done.
cd ../../..
