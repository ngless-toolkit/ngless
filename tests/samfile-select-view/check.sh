#!/usr/bin/env bash

SAMTOOLS="$("${NGLESS_BIN:-ngless}" --print-path samtools)"

if ! diff <($SAMTOOLS view -h output.bam | grep -v '^@PG') <(zcat texpected.sam.gz | grep -v '^@PG') ; then
    exit 1
fi
