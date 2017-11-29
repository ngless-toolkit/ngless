#!/usr/bin/env bash

awk '/^version:/ { print substr($2, 2, length($2) - 2) }' < package.yaml

