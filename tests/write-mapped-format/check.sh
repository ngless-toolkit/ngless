#!/usr/bin/env bash
set -e

# `out_as_sam.bam` was written with format={sam}: despite the .bam name it must be
# a plain-text SAM file, so its first byte is '@' (the SAM header).
first=$(head -c 1 out_as_sam.bam)
if [ "$first" != "@" ]; then
    echo "out_as_sam.bam is not a text SAM file (first byte: '$first')" >&2
    exit 1
fi
# And samtools must round-trip the SAM records unchanged.
if ! diff <(grep -v '^@PG' input.sam) <(samtools view -h out_as_sam.bam | grep -v '^@PG'); then
    echo "out_as_sam.bam does not match input.sam" >&2
    exit 1
fi

# `out_as_bam.dat` was written with format={bam}: despite the .dat name it must be a
# binary BGZF/BAM file (magic byte 0x1f), not text.
magic=$(od -An -N1 -tx1 out_as_bam.dat | tr -d ' ')
if [ "$magic" != "1f" ]; then
    echo "out_as_bam.dat is not a BGZF/BAM file (magic: '$magic')" >&2
    exit 1
fi
if ! diff <(grep -v '^@PG' input.sam) <(samtools view -h out_as_bam.dat | grep -v '^@PG'); then
    echo "out_as_bam.dat does not match input.sam" >&2
    exit 1
fi
