#!/usr/bin/env bash

if [ "$(which samtools 2>/dev/null)" == "" ]; then
    SAMTOOLS="$NGLESS_SAMTOOLS_BIN"
else
    SAMTOOLS="$(which samtools)"
fi

if ! diff <($SAMTOOLS view -h output.bam) texpected.sam ; then
    exit 1
fi
