#!/usr/bin/env bash

if ! diff <(grep -v '^@PG' output.plus.sam) <(grep -v '^@PG' texpected.plus.sam); then
    exit 1
fi
