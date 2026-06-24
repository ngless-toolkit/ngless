#!/usr/bin/env bash

set -e

SAMTOOLS="$("${NGLESS_BIN:-ngless}" --print-path samtools)"

if test x$SAMTOOLS = x ; then
    echo "samtools not found, cannot run test"
    exit 1
fi

echo "Comparing SAM"
diff -q <(grep -v '^@[PG|HD]' output.sam) texpected.sam
echo "Comparing SAM.gz"
diff -q <(zcat output.sam.gz | grep -v '^@[PG|HD]') texpected.sam
echo "Comparing SAM.gz (compress level=0)"
diff -q <(zcat output.c0.sam.gz | grep -v '^@[PG|HD]') texpected.sam
echo "Comparing SAM.bz2"
diff -q <(bzcat output.sam.bz2 | grep -v '^@[PG|HD]') texpected.sam
echo "Comparing SAM.zstd"
diff -q <(zstdcat output.sam.zstd | grep -v '^@[PG|HD]') texpected.sam
echo "Comparing BAM"
diff -q <($SAMTOOLS view -h output.bam | grep -v '^@[PG|HD]') texpected.sam
