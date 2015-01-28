#!/bin/bash

make check

basedir=$PWD
for testdir in tests/*; do
    if test -d $testdir; then 
        echo "Running $testdir"
        cd $testdir
        mkdir -p temp
        ../../dist/build/ngless/ngless -t temp *.ngl
        rm -rf temp
        for f in expected.*; do
            diff -u $f output${f#expected}
        done
        rm -rf *.output_ngless
        cd $basedir
    fi
done

echo "All done."
