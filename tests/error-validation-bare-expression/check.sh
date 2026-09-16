#!/usr/bin/env bash
# Must be reported as an ordinary script error, not as an internal error.
if grep -q "please report this bug\|github.com/ngless-toolkit/ngless/issues" output.stderr.txt; then
    echo "check failed: a bad user script was reported as an internal ngless bug"
    exit 1
fi
if ! grep -q "Line 7: This expression is not a statement" output.stderr.txt; then
    echo "check failed: expected 'not a statement' validation error not found on stderr"
    exit 1
fi
exit 0
