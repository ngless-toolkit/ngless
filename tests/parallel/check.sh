#!/usr/bin/env bash

zcat compressed.tsv.gz | diff -q - expected.tsv
