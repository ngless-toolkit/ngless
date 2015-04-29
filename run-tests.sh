#!/bin/bash

make check

basedir=$PWD
ok="yes"
for testdir in tests/*; do
    if test -d $testdir; then
        echo "Running $testdir"
        cd $testdir
        mkdir -p temp
        ../../dist/build/ngless/ngless -t temp *.ngl
        rm -rf temp
        for f in expected.*; do
            diff -u $f output${f#expected}
            if test $? -ne "0"; then
               echo "ERROR IN TEST"
               ok=no
            fi
        done
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
