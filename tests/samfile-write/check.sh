#!/usr/bin/env bash

if ! diff <(samtools view -h output.bam) texpected.sam ; then
    exit 1
fi
