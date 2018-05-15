#!/usr/bin/env bash

EXPECTED="$(pwd)/somedir/input.fq"

if [ "$1" != "$EXPECTED" ]; then
    echo "1st argument expected '$EXPECTED' saw '$1'"
    exit 1
fi

EXPECTED="--path2=somedir/input2.fq"

if [ "$2" != "$EXPECTED" ]; then
    echo "2nd argument expected '$EXPECTED' saw '$2'"
    exit 1
fi
