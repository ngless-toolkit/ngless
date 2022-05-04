#!/usr/bin/env bash

if [ "$(which samtools 2>/dev/null)" == "" ]; then
    if [ "$NGLESS_SAMTOOLS_BIN" == "" ]; then
        SAMTOOLS="$(ngless --print-path samtools)"
    else
        SAMTOOLS="$NGLESS_SAMTOOLS_BIN"
    fi
else
    SAMTOOLS="$(which samtools)"
fi

if ! diff <($SAMTOOLS view -h output.bam | grep -v '^@PG') <(zcat texpected.sam.gz | grep -v '^@PG') ; then
    exit 1
fi
