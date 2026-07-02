#!/usr/bin/env bash
# The read-set check (addRSChecks) floats up to just after `input = samples[0]`, so the run must
# abort *before* the `println("REACHED_MARKER")` on the next line ever executes.
if grep -q REACHED_MARKER output.stdout.txt; then
    echo "check failed: statement after the read-set assignment ran; the readset check was not early"
    exit 1
fi
if ! grep -q "Cannot read file 'missing_one.gz' for sample 'sample'" output.stderr.txt; then
    echo "check failed: expected unreadable-readset error message not found on stderr"
    exit 1
fi
exit 0
