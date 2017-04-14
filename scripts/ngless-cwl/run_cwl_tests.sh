#!/usr/bin/env bash

FAILURES=0

# WORKAROUND: For these tests to work as in production we'd need an ngless binary with embedded bwa and samtools
# Since compiling this in Travis is currently difficult, we point ngless to the binary location
export NGLESS_SAMTOOLS_BIN="$(readlink -f ../../samtools-1.4/samtools)"
export NGLESS_BWA_BIN="$(readlink -f ../../bwa-0.7.15/bwa)"

for script in cwl/*.cwl; do
    # --leave-outputs - to leave outputs on the tmpdir created by cwl-runner (we don't need them)
    # --preserve-environment - to ensure ngless is found
    # --debug - in case something fails we have extra info
    cwl-runner --debug --rm-tmpdir --leave-outputs \
        --preserve-environment PATH \
        --preserve-environment NGLESS_SAMTOOLS_BIN \
        --preserve-environment NGLESS_BWA_BIN \
        "$script" "test-data/$(basename "${script%.*}".yml)" || { echo "Running $script failed"; let "FAILURES+=1"; }
done

if [ "$FAILURES" != "0" ]; then
    echo "Some CWL tests failed"
    exit 1
fi
