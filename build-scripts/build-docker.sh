#!/usr/bin/env bash

set -eu

mkdir -p docker
cp -pr Dockerfile docker
cd docker
docker build .
docker run "$(docker build . -q)" bash -c "cat /usr/local/bin/ngless" > ngless.bin
chmod +x ngless.bin
