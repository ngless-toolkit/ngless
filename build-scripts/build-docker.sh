#!/usr/bin/env bash

set -eu

mkdir -p docker
mkdir -p docker/artifacts
cp -pr Dockerfile docker
cd docker
docker build .
docker run -v $PWD/artifacts:/artifacts "$(docker build . -q)" cp /usr/local/bin/ngless /artifacts
