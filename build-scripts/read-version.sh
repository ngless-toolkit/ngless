#!/usr/bin/env bash

gawk '/^version:/ { print substr($2, 2, length($2) - 2) }' < package.yaml

