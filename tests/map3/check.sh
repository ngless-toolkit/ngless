#!/usr/bin/env bash

if ! diff <(grep -v '^@[PG|HD]' output.sam) <(grep -v '^@[PG|HD]' texpected.sam); then
    exit 1
fi
