#!/usr/bin/env bash

if ! diff <(grep -v '^@PG' output.sam) <(grep -v '^@PG' texpected.sam); then
    exit 1
fi

if ! diff <(grep -v '^@PG' output.reused.sam) <(grep -v '^@PG' texpected.reused.sam); then
    exit 1
fi
