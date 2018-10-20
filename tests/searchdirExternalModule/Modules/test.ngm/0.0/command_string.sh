#!/usr/bin/env bash

EXPECTED="<location>/input.fq"

if [ "$1" != "$EXPECTED" ]; then
    echo "1st argument expected '$EXPECTED' saw '$1'"
    exit 1
fi

EXPECTED="--ref=<location>/input2.fq"

if [ "$2" != "$EXPECTED" ]; then
    echo "2nd argument expected '$EXPECTED' saw '$2'"
    exit 1
fi

EXPECTED="--ref2=<fullpath>/input2.fq"

if [ "$3" != "$EXPECTED" ]; then
    echo "3rd argument expected '$EXPECTED' saw '$3'"
    exit 1
fi

EXPECTED="--string=sentence"

if [ "$4" != "$EXPECTED" ]; then
    echo "4nd argument expected '$EXPECTED' saw '$4'"
    exit 1
fi
