#!/usr/bin/env bash

SAMTOOLS="$(ngless --print-path samtools)"

if ! diff <($SAMTOOLS view -h output.bam | grep -v '^@PG') <(zcat texpected.sam.gz | grep -v '^@PG') ; then
    exit 1
fi
