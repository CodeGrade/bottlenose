#!/bin/bash

DIR=$1

if [[ ! $(whoami) == 'root' ]]
then
    echo "Must be run as root"
    exit 1
fi

if [[ $DIR == "" ]]
then
    echo "Usage: $0 directory"
    exit 2
fi

CWD=`pwd`
if [[ $CWD != "/tmp"* ]]
then
    echo "Target isn't in /tmp:"
    echo "  " $CWD
    exit 3
fi

mkdir $DIR
mount -t tmpfs -o size=512m tmpfs $DIR

cd $DIR

for dd in usr bin lib var etc proc dev
do
    mkdir $dd
    mount --bind /$dd $dd
    #mount -o remount,ro $dd
done

if [[ -d "/lib64" ]]
then
    mkdir lib64
    mount --bind /lib64 lib64
    #mount -o remount,ro lib64
fi

mkdir home
mkdir home/student
chmod 777 home/student

mkdir tmp
chmod 777 tmp

if [[ -e "../grading.tar.gz" ]]
then
    cp ../grading.tar.gz home/student
    (cd home/student && tar xzvf grading.tar.gz)
fi

if [[ -e "../sub.tar.gz" ]]
then
    cp ../sub.tar.gz home/student

    CONFIG=`find home/student -type d -name ".bottlenose"`
    COUNT=`echo $CONFIG | wc -w`

    if [[ $COUNT -eq 1 ]]
    then
        echo "Instructor config found."
    else
        (cd home/student && tar xzvf sub.tar.gz)
        HAX=`find home/student -type d -name ".bottlenose" | wc -l`
        if [[ $HAX -gt 0 ]]
        then
            echo "Found a new .bottlenose directory in the student submission."
            echo "HAX!"
            exit 1
        fi
    fi
fi

