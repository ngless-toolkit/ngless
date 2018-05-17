#!/usr/bin/env bash

if ! diff <(grep -v '^@PG' output1.sam) <(grep -v '^@PG' texpected.sam); then
    exit 1
fi

if ! diff <(grep -v '^@PG' output2.sam) <(grep -v '^@PG' texpected.sam); then
    exit 1
fi
