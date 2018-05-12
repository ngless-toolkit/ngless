#!/usr/bin/env bash

EXPECTED="$(pwd)/somedir/input.fq"

if [ "$1" != "$EXPECTED" ]; then
    echo "Expected '$EXPECTED' saw '$1'"
    exit 1
fi
