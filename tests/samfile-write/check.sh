#!/usr/bin/env bash

SAMTOOLS="$("${NGLESS_BIN:-ngless}" --print-path samtools)"

if test x$SAMTOOLS = x ; then
    echo "samtools not found, cannot run test"
    exit 1
fi

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

