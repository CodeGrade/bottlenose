#!/bin/bash

BINARY=sandbox
DIR=/usr/local/bottlenose

if [[ ! -e $BINARY ]]; then
    echo "You're going to want to build first."
    exit
fi

if [[ ! $USER == "root" ]]; then
    echo "Install must be done as root for setuid."
    exit
fi

if [[ -d $DIR ]]; then
    rm -rf $DIR
fi

mkdir $DIR
mkdir $DIR/bin

cp $BINARY $DIR/bin
chmod +s $DIR/bin/$BINARY

cp -r ../scripts $DIR
