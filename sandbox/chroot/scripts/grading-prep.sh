#!/bin/bash

cd $1

CONFIG=`find . -type d -name ".bottlenose"`
COUNT=`echo $CONFIG | wc -w`

if [[ $COUNT -eq 1 ]]
then
    if [[ -e $CONFIG/prep ]]
    then
        (cd `dirname $CONFIG` && .bottlenose/prep)
    fi
else
    echo "Falling back to old rules - re-unpacking grading tarball."
    if [[ -e "../grading.tar.gz" ]]
    then
        cp ../grading.tar.gz home/student
        (cd home/student && tar xzvf grading.tar.gz)
    fi
fi
