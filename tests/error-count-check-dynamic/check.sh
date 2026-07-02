#!/usr/bin/env bash
# The functional_map is a variable, so validate_count skips it and the runtime __check_count
# (addCountsCheck) is what catches the missing feature. That check runs before count()/write(),
# so the write output must never be created...
if test -f should.not.be.created.txt; then
    echo "check failed: write() ran before the count check aborted the script"
    exit 1
fi
# ...and the message is the plain executeCountCheck one (line 10), not the richer static-check
# message (which also lists "Available columns are:").
if ! grep -q "In call to count() \[line 10\], missing features: kot" output.stderr.txt; then
    echo "check failed: expected missing-features error message not found on stderr"
    exit 1
fi
if grep -q "Available columns are:" output.stderr.txt; then
    echo "check failed: got the static-validation message, not the runtime __check_count one"
    exit 1
fi
exit 0
