#!/usr/bin/env bash

[ "$(tail -n 1 output.stdout.txt)" != "projectA/sampleA1" ] && exit 1

exit 0
