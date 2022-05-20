#!/usr/bin/env bash

SAMTOOLS="$(ngless --print-path samtools)"

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
