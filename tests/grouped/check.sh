#!/usr/bin/env bash

if diff <(grep -v '^@PG' one.sam) <(grep -v '^@PG' split.sam ) >/dev/null; then
    exit 1
fi
rm one.sam split.sam
