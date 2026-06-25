#!/usr/bin/env bash

exec diff <(grep -v '^@PG' output.sam) <(grep -v '^@PG' texpected.sam)
