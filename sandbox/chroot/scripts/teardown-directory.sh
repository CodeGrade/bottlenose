#!/bin/bash

DIR=$1

if [[ ! $(whoami) == 'root' ]]
then
    echo "Must be run as root"
    exit
fi

if [[ ! -d "$DIR" ]]
then
    echo "Usage: $0 directory"
    exit
fi

cd $DIR

for dd in usr bin lib var etc proc dev
do
    umount $dd
    rmdir $dd
done

if [[ -d "lib64" ]]
then
    umount lib64
    rmdir lib64
fi

rm -rf --one-file-system home/student
rmdir home

cd ..
#rm -rf $DIR/tmp
umount $DIR
rmdir $DIR
