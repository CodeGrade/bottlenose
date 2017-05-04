#!/bin/bash

unset RUBYOPT
unset BUNDLE_GEMFILE
unset BUNDLE_BIN_PATH

CONFIG=`find . -type d -name ".bottlenose"`
COUNT=`echo $CONFIG | wc -w`

if [[ $COUNT -eq 1 ]]
then
    if [[ -e $CONFIG/grade ]]
    then
        (cd `dirname $CONFIG` && .bottlenose/grade)
    fi
elif [[ $COUNT -eq 0 ]]
then
    MAKEFILE=`find . -name Makefile`
    MF_COUNT=`echo $MAKEFILE | wc -w`

    if [[ $MF_COUNT -ne 1 ]]
    then
        echo "Need exactly 1 Makefile in directory tree."
        echo "Found $COUNT instead, giving up."
        exit 1
    fi

    (cd `dirname $MAKEFILE` && make test)
else
    echo "No grading scripts."
fi
