#!/usr/bin/env bash

##  TODO: Location of bluepill-micropython repo.
mpy=$PWD/../bluepill-micropython

##  Location of src folder.
src=$PWD/lib/bluepill-micropython/src

##  Erase the current links in src.
rm -rf $src
mkdir -p $src
cd $src

##  Link the ports/bluepill files into lib/bluepill-micropython/src.
cd $src
pwd; echo "ln -s ../ports/bluepill/* ."
ln -s ../ports/bluepill/* .

link_folder() {
    ##  $1 is the name of the folder in bluepill-micropython, e.g. "py".
    dir=$1
    src_dir=$src/$dir
    mpy_dir=$mpy/$dir

    ##  Link the bluepill-micropython/???/*.c files into lib/bluepill-micropython/src.
    cd $src
    pwd; echo "ln -s $mpy_dir/*.c ."
    ## ls -l $mpy_dir/*.c
    ln -s $mpy_dir/*.c .

    ##  Link the bluepill-micropython/???/*.h files into lib/bluepill-micropython/src/???.
    mkdir -p $src_dir
    cd $src_dir
    pwd; echo "ln -s $mpy_dir/*.h ."
    ## ls -l $mpy_dir/*.h .
    ln -s $mpy_dir/*.h .
}

##  Handle each folder.
link_folder py
link_folder extmod

##  Done
