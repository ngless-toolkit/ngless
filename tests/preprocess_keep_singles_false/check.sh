#!/usr/bin/env bash

# With keep_singles=False, orphaned mates must be discarded, so no singles
# file may be produced.
if test -e output.singles.fq; then
    >&2 echo "output.singles.fq should not exist with keep_singles=False"
    exit 1
fi
