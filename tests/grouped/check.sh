#!/usr/bin/env bash

if ! diff <(grep -v '^@PG' one.sam) <(grep -v '^@PG' split.sam ) ; then
    exit 1
fi
rm -f one.sam split.sam
