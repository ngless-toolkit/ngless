#!/usr/bin/env bash

for out in output.from.bam.sam.gz output.sam.gz; do
    if ! diff input.sam <(zcat ${out}); then
        echo "Difference between ${out} and input.sam" >&2
        exit 1
    fi
done
