#!/usr/bin/env bash

set -eo pipefail
zcat compressed.tsv.gz | diff -q - expected.tsv
