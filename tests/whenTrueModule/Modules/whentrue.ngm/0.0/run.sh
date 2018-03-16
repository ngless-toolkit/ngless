#!/usr/bin/env bash

if [ "$2" != "--whentrueworks" ]; then 
    echo "Expected --whentrueworks saw $2"
    exit 1
fi
