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

if ! diff <($SAMTOOLS view -h output.unsorted.bam) texpected.unsorted.sam ; then
    exit 1
fi

if ! diff <($SAMTOOLS view -h output.pos_sorted.bam) texpected.pos_sorted.sam ; then
    exit 1
fi

if ! diff <($SAMTOOLS view -h output.pos_sorted2.bam) texpected.pos_sorted.sam ; then
    exit 1
fi

if ! diff <($SAMTOOLS view -h output.name_sorted.bam) texpected.name_sorted.sam ; then
    exit 1
fi
