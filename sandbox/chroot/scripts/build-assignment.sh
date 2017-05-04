#!/bin/bash

CONFIG=`find . -type d -name ".bottlenose"`
COUNT=`echo $CONFIG | wc -w`

if [[ $COUNT -eq 0 ]]
then
    echo "Falling back to old rule: Make exactly one Makefile"

    MAKEFILE=`find . -type f -name "Makefile"`
    MF_COUNT=`echo $MAKEFILE | wc -w`
    if [[ $MF_COUNT -eq 1 ]]
    then
        (cd `dirname $MAKEFILE` && make)
        HAX=`find . -type d -name ".bottlenose" | wc -l`
        if [[ $HAX -gt 0 ]]
        then
            echo "Found a new .bottlenose directory in the student submission."
            echo "HAX!"
            exit 1
        fi
    else
        echo "Bad submissions state - testing directory has multiple Makefiles:"
        echo $MAKEFILE
    fi
elif [[ $COUNT -eq 1 ]]
then
    if [[ -e "$CONFIG/build" ]]
    then
        (cd `dirname $CONFIG` && .bottlenose/build `readlink -f ..`/sub.tar.gz)
    fi
else
    echo "Found too many .bottlenose directories:"
    echo
    echo $CONFIG
    echo
    echo "Skipping build step."
fi
