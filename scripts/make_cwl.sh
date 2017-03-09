#!/usr/bin/env bash
# make_cwl.sh -- created 2017-03-09 - Renato Alves

set -e

for script in ngless-*.py; do
    ./"$script" --generate_cwl_tool
done

# vi: 
