#!/usr/bin/env bash

set -e

PYTHON=python

dirname=HTSeq-0.6.1
fname=$dirname.tar.gz

url=http://pypi.python.org/packages/source/H/HTSeq/$fname#md5=b7f4f38a9f4278b9b7f948d1efbc1f05

download(){
  echo "start downloading" $2
  wget -O $1 $2 &>/dev/null
  r=$?
  if [ $r != 0 ] ; then
    rm -rf $1
    echo "ERROR downloading " $2 "."
    exit 3
  fi
  echo "ended downloading" $1
}


downloadURL(){
    if [ ! -d "$dirname" ]; then
        download $fname $1
        echo tar -xzvf $fname
        tar -xzvf $fname
        cd $dirname; $PYTHON setup.py install --user;
        if [ $r != 0 ]; then
            rm -rf $1
            echo "ERROR installing HTSEQ."
            exit 3
        fi
        cd ..
        echo "[HTSEQ] installed"
    else
        echo "[HTSEQ] Already installed"
    fi
    rm -rf $fname
}

downloadURL $url


