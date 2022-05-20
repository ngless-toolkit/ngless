#!/usr/bin/env bash

SAMTOOLS="$(ngless --print-path samtools)"

failed=0
if ! diff <($SAMTOOLS view -h output.1.bam | grep -v '^@PG') texpected.sam ; then
    failed=1
fi


if ! diff <(zcat output.2.sam.gz | grep -v '^@PG') texpected.sam ; then
    failed=1
fi

if ! diff <(zcat output.3.sam.gz | grep -v '^@PG') texpected.sam ; then
    failed=1
fi

exit $failed

