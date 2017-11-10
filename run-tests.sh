#!/usr/bin/env bash

shopt -s nullglob

SAMTOOLS_VERSION=1.6
BWA_VERSION=0.7.15
MEGAHIT_VERSION=1.1.1
PRODIGAL_VERSION=2.6.3

SAMTOOLS_BIN=ngless-samtools
BWA_BIN=ngless-bwa
PRODIGAL_BIN=ngless-prodigal
MEGAHIT_BIN=megahit

function remove_ngless_bin {
    # Ensure that the directory where ngless unpacks embedded binaries is removed (we want to test the embedded blobs)
    NGLDIR="$HOME/.local/share/ngless/bin"
    [ -d "$NGLDIR" ] && echo "Removing $NGLDIR to ensure embedded bins are used" && rm -rf "$NGLDIR"
}

# This script is located on the root of the repository
# where samtools and bwa are also compiled
REPO="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo ">>> Using repository at $REPO <<<"

if [[ "$0" == *-static.sh ]]; then
    echo ">>> Testing NGLess ( static build with embedded binaries ) <<<"
    MAKETARGET="static"
else
    echo ">>> Testing NGLess ( regular build ) <<<"
    export NGLESS_SAMTOOLS_BIN=$REPO/samtools-${SAMTOOLS_VERSION}/${SAMTOOLS_BIN}
    export NGLESS_BWA_BIN=$REPO/bwa-${BWA_VERSION}/${BWA_BIN}
    export NGLESS_PRODIGAL_BIN=$REPO/Prodigal-${PRODIGAL_VERSION}/${PRODIGAL_BIN}
    export NGLESS_MEGAHIT_BIN=$REPO/megahit-${MEGAHIT_VERSION}/${MEGAHIT_BIN}
    echo ">> Will use samtools from '$NGLESS_SAMTOOLS_BIN' <<"
    make samtools-${SAMTOOLS_VERSION}/${SAMTOOLS_BIN} || (echo "make samtools failed" ; exit 1)
    echo ">> Will use bwa from '$NGLESS_BWA_BIN' <<"
    make bwa-${BWA_VERSION}/${BWA_BIN} || (echo "make bwa failed" ; exit 1)
    echo ">> Will use prodigal from '$NGLESS_PRODIGAL_BIN' <<"
    make Prodigal-${PRODIGAL_VERSION}/${PRODIGAL_BIN} || (echo "make prodigal failed" ; exit 1)
    echo ">> Will use megahit from '$NGLESS_MEGAHIT_BIN' <<"
    make megahit-${MEGAHIT_VERSION}/${MEGAHIT_BIN} || (echo "make megahit failed" ; exit 1)
    MAKETARGET=""
fi

# haskell unit tests
echo "Running 'make check'"
make check
if test $? -ne "0"; then
    echo "ERROR IN 'make check'"
    exit 1
fi

# ngless modules - needed for tests later on
echo "Running 'make modules'"
make modules
if test $? -ne "0"; then
    echo "ERROR IN 'make modules'"
    exit 1
fi

# Our test target (if not the default) is then also used for the subsequent tests
if [ "$MAKETARGET" != "" ]; then
    echo "Running 'make $MAKETARGET'"
    make $MAKETARGET
    if test $? -ne "0"; then
        echo "ERROR IN 'make $MAKETARGET'"
        exit 1
    fi
else
    if ! test -x "$NGLESS_SAMTOOLS_BIN" ; then
        echo "ERROR: samtools not found at '$NGLESS_SAMTOOLS_BIN'"
        exit 1
    fi
    if ! test -x "$NGLESS_BWA_BIN" ; then
        echo "ERROR: bwa not found at '$NGLESS_BWA_BIN'"
        exit 1
    fi
    if ! test -x "$NGLESS_MEGAHIT_BIN" ; then
        echo "ERROR: bwa not found at '$NGLESS_MEGAHIT_BIN'"
        exit 1
    fi

    echo "running 'make'"
    make
    if test $? -ne "0"; then
        echo "error in 'make'"
        exit 1
    fi
fi

ngless_bin=$(stack path ${STACKOPTS} --local-install-root)/bin/ngless
if ! test -x "$ngless_bin" ; then
    echo "Could not determine path for ngless (guessed $ngless_bin)"
    exit 1
fi

echo ">>> Running tests with: $($ngless_bin --version-debug) <<<"

if [ "$MAKETARGET" != "" ]; then
    echo ">> Checking that binaries are indeed embedded"
    remove_ngless_bin

    $ngless_bin --print-path bwa &>/dev/null
    if test $? -ne "0"; then
        echo "Error 'bwa' binary was not correctly embedded"
        exit 1
    fi
    $ngless_bin --print-path samtools &>/dev/null
    if test $? -ne "0"; then
        echo "Error 'samtools' binary was not correctly embedded"
        exit 1
    fi
    echo ">> Binaries were correctly embedded"
fi

# To run ./check.sh on tests and to test ngless_cwl wrappers, need to add ngless to PATH
PATH="$(dirname "$ngless_bin"):$PATH"
export PATH

ok="yes"
basedir=$REPO
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
    echo "An error occurred."
    exit 1
fi
