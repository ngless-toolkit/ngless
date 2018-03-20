#!/usr/bin/env bash
set -e

shopt -s nullglob

if ! test -f test_samples/functional.map; then
    gunzip --keep test_samples/functional.map.gz
fi
if ! test -f test_samples/sample.sam; then
    gunzip --keep test_samples/sample.sam
fi
if ! test -f test_samples/merge/sp_sample_127; then
    echo "Generating fake profiles for merging"
    (cd test_samples/merge && python generate.py)
fi
exec stack bench --benchmark-arguments "--output bench.html"
