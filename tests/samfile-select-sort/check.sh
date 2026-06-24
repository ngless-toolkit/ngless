#!/usr/bin/env bash

SAMTOOLS="$("${NGLESS_BIN:-ngless}" --print-path samtools)"

if test x$SAMTOOLS = x ; then
    echo "samtools not found, cannot run test"
    exit 1
fi

failed=0
if ! diff <($SAMTOOLS view -h output.unsorted.bam | grep -v '^@PG') texpected.unsorted.sam ; then
    failed=1
fi

if ! diff <($SAMTOOLS view -h output.pos_sorted.bam | grep -v '^@PG') texpected.pos_sorted.sam ; then
    failed=1
fi

if ! diff <($SAMTOOLS view -h output.pos_sorted2.bam | grep -v '^@PG') texpected.pos_sorted.sam ; then
    failed=1
fi

if ! diff <($SAMTOOLS view -h output.name_sorted.bam | grep -v '^@PG') texpected.name_sorted.sam ; then
    failed=1
fi

exit $failed
