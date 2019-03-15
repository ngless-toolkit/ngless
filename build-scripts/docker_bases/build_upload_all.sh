#!/usr/bin/env bash

echo "Building alpine"
( cd alpine && ./build.sh ) &> alpine-build.log
echo "Uploading alpine"
( cd alpine && ./upload.sh ) &> alpine-upload.log

echo "Building ubuntu-devel"
( cd ubuntu-devel && ./build.sh ) &> ubuntu-devel-build.log
echo "Uploading ubuntu-devel"
( cd ubuntu-devel && ./upload.sh ) &> ubuntu-devel-upload.log

echo "Building ubuntu-old"
( cd ubuntu-old && ./build.sh ) &> ubuntu-old-build.log
echo "Uploading ubuntu-old"
( cd ubuntu-old && ./upload.sh ) &> ubuntu-old-upload.log

echo "Building ubuntu-ghc-wsl"
( cd ubuntu-ghc-wsl && ./build.sh ) &> ubuntu-ghc-wsl-build.log
echo "Uploading ubuntu-ghc-wsl"
( cd ubuntu-ghc-wsl && ./upload.sh ) &> ubuntu-ghc-wsl-upload.log
