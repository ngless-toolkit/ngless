#!/usr/bin/env bash

shopt -s nullglob

SAMTOOLS_VERSION=1.4
BWA_VERSION=0.7.15

function remove_ngless_bin {
    # Ensure that the directory where ngless unpacks embedded binaries is removed (we want to test the embedded blobs)
    NGLDIR="$HOME/.local/share/ngless/bin"
    [ -d "$NGLDIR" ] && echo "Removing $NGLDIR to ensure embedded bins are used" && rm -rf "$NGLDIR"
}

# This script is located on the root of the repository
# where samtools and bwa are also compiled
REPO="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo ">>> Using repository at $REPO <<<"

if [[ "$0" == *-embedded.sh ]]; then
    echo ">>> Testing NGLess ( with embedded binaries ) <<<"
    MAKETARGET="ngless-embed"
    remove_ngless_bin
elif [[ "$0" == *-static.sh ]]; then
    echo ">>> Testing NGLess ( static build with embedded binaries ) <<<"
    MAKETARGET="static"
    remove_ngless_bin
else
    echo ">>> Testing NGLess ( regular build ) <<<"
    export NGLESS_SAMTOOLS_BIN=$REPO/samtools-${SAMTOOLS_VERSION}/samtools
    export NGLESS_BWA_BIN=$REPO/bwa-${BWA_VERSION}/bwa
    echo ">> Will use samtools from '$NGLESS_SAMTOOLS_BIN' <<"
    make samtools-${SAMTOOLS_VERSION}/samtools || (echo "make samtools failed" ; exit 1)
    echo ">> Will use bwa from '$NGLESS_BWA_BIN' <<"
    make bwa-${BWA_VERSION}/bwa || (echo "make bwa failed" ; exit 1)
    MAKETARGET=""
fi

ok="yes"

make
if test $? -ne "0"; then
    echo "ERROR IN 'make'"
    ok=no
fi
# haskell unit tests
make check
if test $? -ne "0"; then
    echo "ERROR IN 'make check'"
    ok=no
fi

# Our test target (if not the default) is then also used for the subsequent tests
if [ "$MAKETARGET" != "" ]; then
    make $MAKETARGET
    if test $? -ne "0"; then
        echo "ERROR IN 'make $MAKETARGET'"
        ok=no
    fi
else
    if ! test -x $NGLESS_SAMTOOLS_BIN ; then
        echo "ERROR: samtools not found at '$NGLESS_SAMTOOLS_BIN'"
        exit 1
    fi
    if ! test -x $NGLESS_BWA_BIN ; then
        echo "ERROR: bwa not found at '$NGLESS_BWA_BIN'"
        exit 1
    fi
fi

ngless_bin=$(stack path --local-install-root)/bin/ngless
if ! test -x $ngless_bin ; then
    echo "Could not determine path for ngless (guessed $ngless_bin)"
    exit 1
fi

echo ">>> Running with: $($ngless_bin --version-debug) <<<"

basedir=$REPO
for testdir in tests/*; do
    if test -d $testdir; then
        cur_ok=yes
        if test -f ${testdir}/TRAVIS_SKIP -a x$TRAVIS = xtrue; then
            echo "Skipping $testir on Travis"
            continue
        fi
        echo "Running $testdir"
        cd $testdir
        mkdir -p temp
        validate_arg=""
        if [[ $testdir == tests/error-validation-* ]] ; then
            validate_arg="-n"
        fi
        cmd_args=""
        if test -f cmdargs ; then
            cmd_args="$(cat cmdargs)"
        fi
        $ngless_bin --quiet -t temp $cmd_args $validate_arg *.ngl > output.stdout.txt 2>output.stderr.txt
        ngless_exit=$?
        if [[ $testdir == tests/error-* ]] ; then
            if test $ngless_exit -eq "0"; then
                echo "Expected error message in test"
                cur_ok=no
            fi
        else
            if test $ngless_exit -ne "0"; then
                echo "Error exit in test"
                cur_ok=no
            fi
        fi
        for f in expected.*; do
            out=output${f#expected}
            diff -u $f $out
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
        fi

        if test -x ./cleanup.sh; then
            ./cleanup.sh
        fi
        rm -rf temp
        rm -rf *.output_ngless
        rm -f output.*
        cd $basedir
    fi
done

if test $ok = "yes"; then
    echo "All done."
else
    echo "An error occurred."
    exit 1
fi
