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

if ! diff <($SAMTOOLS view -h output.1.bam) texpected.sam ; then
    exit 1
fi


if ! diff <(zcat output.2.sam.gz) texpected.sam ; then
    exit 1
fi

if ! diff <(zcat output.3.sam.gz) texpected.sam ; then
    exit 1
fi


