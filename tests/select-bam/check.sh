#!/usr/bin/env bash

if ! diff <(samtools view output.neg.bam) texpected.neg.sam ; then
    exit 1
fi
if ! diff <(samtools view output.plus.bam) texpected.plus.sam ; then
    exit 1
fi
