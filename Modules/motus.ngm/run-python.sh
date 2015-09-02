#!/bin/bash

if ! which python >/dev/null ; then
    echo "python command not found"
    exit 1;
fi

if [[ ! -z "$1" ]] ; then
    python motus-summary.py $1 > $2
fi
