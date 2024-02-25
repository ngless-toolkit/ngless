#!/usr/bin/env bash

if ! diff <(grep -v '^@[PG|HD]' output.plus.sam) <(grep -v '^@[PG|HD]' texpected.plus.sam); then
    exit 1
fi
