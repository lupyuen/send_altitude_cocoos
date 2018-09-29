#!/usr/bin/env bash
mkdir lib
cd lib
mkdir bluepill-micropython
cd bluepill-micropython
mkdir src
cd src

mpy=../../../../bluepill-micropython
dir=py

## ln -s $mpy/$dir/*.c .

mkdir $dir
cd $dir

ls -l ../$mpy/$dir/*.h
ln -s ../$mpy/$dir/*.h .

cd ..
cd ../../..
