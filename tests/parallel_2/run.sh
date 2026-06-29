#!/usr/bin/env bash

set -e

for i in {1..2}; do
    $NGLESS_BIN --quiet -t temp parallel.ngl
    if [ $? -ne 0 ]; then
        echo "Error: ngless failed to run parallel.ngl"
        exit 1
    fi
done
