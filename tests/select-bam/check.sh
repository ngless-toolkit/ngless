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

if ! diff <($SAMTOOLS view output.neg.bam) texpected.neg.sam ; then
    exit 1
fi
if ! diff <($SAMTOOLS view output.plus.bam) texpected.plus.sam ; then
    exit 1
fi
