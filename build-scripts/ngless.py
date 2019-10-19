#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from subprocess import run, PIPE, STDOUT


def has_collect(output):
    collect = "Cannot collect (not all files present yet)"

    for line in output.split("\n"):
        if collect in line:
            return True


def ngless():
    cmd = ["ngless", "--trace"] + sys.argv[1:]
    while True:
        print(">>>> Calling ngless")
        p = run(cmd, check=True, stdout=PIPE, stderr=STDOUT)

        stdout = p.stdout.decode("utf8")
        print(stdout)

        if not has_collect(stdout):
            break


if __name__ == "__main__":
    ngless()
