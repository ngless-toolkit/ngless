#!/usr/bin/env bash

shopt -s nullglob

ok="yes"

make
if test $? -ne "0"; then
   echo "ERROR IN 'make'"
   ok=no
fi
make check
if test $? -ne "0"; then
   echo "ERROR IN 'make check'"
   ok=no
fi

basedir=$PWD
for testdir in tests/*; do
    if test -d $testdir; then
        echo "Running $testdir"
        cd $testdir
        mkdir -p temp
        ../../dist/build/ngless/ngless --quiet -t temp *.ngl > output.stdout.txt 2>output.stderr.txt
        ngless_exit=$?
        if [[ $testdir == tests/error-* ]] ; then
            if test $ngless_exit -eq "0"; then
                echo "Expected error message in test"
                ok=no
            fi
        else
            if test $ngless_exit -ne "0"; then
                echo "Error exit in test"
                ok=no
            fi
        fi
        for f in expected.*; do
            diff -u $f output${f#expected}
            if test $? -ne "0"; then
               echo "ERROR IN TEST"
               ok=no
            fi
        done
        rm -rf temp
        rm -rf *.output_ngless
        cd $basedir
    fi
done

if test $ok = "yes"; then
    echo "All done."
else
    echo "An error occurred."
    exit 1
fi
