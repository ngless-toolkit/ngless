#!/usr/bin/env bash

set -e

SAMTOOLS="$(ngless --print-path samtools)"

echo "Comparing SAM"
diff -q <(grep -v '^@PG' output.sam) expected.sam
echo "Comparing SAM.gz"
diff -q <(zcat output.sam.gz | grep -v '^@PG') expected.sam
echo "Comparing SAM.bz2"
diff -q <(bzcat output.sam.bz2 | grep -v '^@PG') expected.sam
echo "Comparing SAM.zstd"
diff -q <(zstdcat output.sam.zstd | grep -v '^@PG') expected.sam
echo "Comparing BAM"
diff -q <($SAMTOOLS view -h output.bam | grep -v '^@PG') expected.sam
