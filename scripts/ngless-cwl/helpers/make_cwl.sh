#!/usr/bin/env bash
# make_cwl.sh -- created 2017-03-09 - Renato Alves

set -e

echo "This script is used to create a template .cwl file from existing scripts"
echo "the final result will be incomplete. You'll need to manually adjust"
echo "output: and type: File for inputs:"

export PYTHONPATH="../:$PYTHONPATH"

for script in ../bin/ngless-*.py; do
    name="$(basename "$script")"
    if [ -f "../cwl/${name%.*}.cwl" ]; then
        echo "Skipping $name - CWL description already exists"
    else
        PATH=../bin:$PATH "$name" --generate_cwl_tool
    fi
done
