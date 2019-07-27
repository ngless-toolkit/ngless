#!/usr/bin/env bash

if test -f should.not.be.created.txt; then
    exit 1
else
    exit 0
fi
