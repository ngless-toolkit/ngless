#!/usr/bin/env bash

shopt -s nullglob

# This script is located on the root of the repository:
basedir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Check that ngless is in the PATH and correctly installed:
if ! ngless --check-install ; then
    echo "$(which ngless) --check-install failed."
    exit 1
fi

echo ">>> Running tests with: $(ngless --version-debug) <<<"

ok="yes"
failed_tests=""
for testdir in tests/*; do
    if test -d "$testdir"; then
        cur_ok=yes
        if test -f "${testdir}/TRAVIS_SKIP" -a "x$TRAVIS" = xtrue; then
            echo "Skipping $testdir on Travis"
            continue
        fi
        echo "Running $testdir"
        cd "$testdir"
        mkdir -p temp
        validate_arg=""
        if [[ $testdir == tests/error-validation-* ]] ; then
            validate_arg="-n"
        fi
        cmd_args=""
        if test -f cmdargs ; then
            cmd_args="$(cat cmdargs)"
        fi
        ngless --quiet -t temp $cmd_args $validate_arg *.ngl > output.stdout.txt 2>output.stderr.txt
        ngless_exit=$?
        if [[ $testdir == tests/error-* ]] ; then
            if test $ngless_exit -eq "0"; then
                echo "NGLess exited with exit code 0, even though an error was expected in test"
                cur_ok=no
            fi
        else
            if test $ngless_exit -ne "0"; then
                echo "Error non-zero exit in test: $testdir"
                cur_ok=no
            fi
        fi
        for f in expected.*; do
            out=output${f#expected}
            diff -u "$f" "$out"
            if test $? -ne "0"; then
               echo "ERROR in test $testdir: $out did not match $f"
               cur_ok=no
            fi
        done
        if test -x ./check.sh; then
            ./check.sh
            if test $? -ne "0"; then
                echo "ERROR in test $testdir: ./check.sh failed"
                cur_ok=no
            fi
        fi

        if test $cur_ok = "no"; then
            echo "ERROR: Output from from ngless was:"
            cat output.stdout.txt
            cat output.stderr.txt
            ok=no
            failed_tests="${failed_tests} ${testdir}"
        fi

        if test -x ./cleanup.sh; then
            ./cleanup.sh
        fi
        rm -rf temp
        rm -rf ./*.output_ngless
        rm -f output.*
        cd "$basedir"
    fi
done

if test $ok = "yes"; then
    echo "All done."
else
    echo "The following tests failed:"
    for f in $failed_tests; do
        echo " - $f"
    done
    exit 1
fi
