#!/usr/bin/env bash

EXPECTED="1"

if [ "$1" != "$EXPECTED" ]; then
    echo "1st argument expected '$EXPECTED' saw '$1'"
    exit 1
fi
