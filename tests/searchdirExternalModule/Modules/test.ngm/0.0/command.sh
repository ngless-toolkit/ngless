#!/usr/bin/env bash

if [ "$1" != "/somedir/input.fq" ]; then
    echo "Expected '/somedir/input.fq' saw '$1'"
    exit 1
fi
