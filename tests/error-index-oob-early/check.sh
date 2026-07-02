#!/usr/bin/env bash
# The out-of-bounds check (addIndexChecks) floats up to just after `x = [...]`, so the run must
# abort *before* the `println("REACHED_MARKER")` on the next line ever executes.
if grep -q REACHED_MARKER output.stdout.txt; then
    echo "check failed: statement after the array assignment ran; the index check was not early"
    exit 1
fi
if ! grep -q "Index access on line 5 is invalid" output.stderr.txt; then
    echo "check failed: expected out-of-bounds error message not found on stderr"
    exit 1
fi
exit 0
