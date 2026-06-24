#!/usr/bin/env bash

SAMTOOLS="$("${NGLESS_BIN:-ngless}" --print-path samtools)"

if ! diff <($SAMTOOLS view output.neg.bam) texpected.neg.sam ; then
    exit 1
fi
if ! diff <($SAMTOOLS view output.plus.bam) texpected.plus.sam ; then
    exit 1
fi
