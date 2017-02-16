#!/usr/bin/env bash

if [ "$(which samtools 2>/dev/null)" == "" ]; then
    SAMTOOLS="$NGLESS_SAMTOOLS_BIN"
else
    SAMTOOLS="$(which samtools)"
fi

if ! diff <($SAMTOOLS view output.neg.bam) texpected.neg.sam ; then
    exit 1
fi
if ! diff <($SAMTOOLS view output.plus.bam) texpected.plus.sam ; then
    exit 1
fi
