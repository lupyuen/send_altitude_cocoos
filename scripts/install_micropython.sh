#!/usr/bin/env bash
mkdir lib
cd lib
mkdir bluepill-micropython
cd bluepill-micropython
mkdir src
cd src

mpy=../../../../bluepill-micropython
dir=py

##  Link the ports/bluepill files into lib/bluepill-micropython/src.
ln -s ../ports/bluepill/* .

##  Link the bluepill-micropython/py/*.c files into lib/bluepill-micropython/src.
ln -s $mpy/$dir/*.c .

##  Link the bluepill-micropython/py/*.h files into lib/bluepill-micropython/src/py.
mkdir $dir
cd $dir
ln -s ../$mpy/$dir/*.h .
cd ..

##  Done.
cd ../../..
