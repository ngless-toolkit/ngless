#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

from subprocess import Popen
import sys
import tempfile
from ngless.wrap import ngl_prepare_options, ngl_prepare_payload

try:
    import argparse
except ImportError:
    print("argparse not found. Please install argparse with 'pip install argparse'")
    sys.exit(1)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input", required=True,
                        help="FastQ file to filter")
    parser.add_argument("-o", "--output", required=True,
                        help="Output file/path for results")
    parser.add_argument("-c", "--max-copies",
                        help="Max number of duplicate copies to keep")
    parser.add_argument("--debug", action="store_true",
                        help="Prints the payload before submitting to ngless")

    return parser.parse_args()


def prepare(args):
    # NOTE this needs to match the arguments in parse_args and the targets in payload
    # commas at the beginning of each option are used when that section has
    # parameters from other functions.
    options = {
        "input_opts": {
            "input": "'{input}'",
        },
        "write_opts": {
            "output": ", ofile='{output}'",
        },
        "target_opts": {
            "max_copies": ", max_copies={max_copies}",
        },
    }

    ngl_options = ngl_prepare_options(args, options)

    payload_tpl = """\
ngless "0.0"
input = fastq({input_opts})
unique = unique(input{target_opts})
write(unique{write_opts})
""".format(**ngl_options)

    return ngl_prepare_payload(args, payload_tpl)


def ngless(args):
    payload = prepare(args)

    with tempfile.NamedTemporaryFile() as script:
        script.write(payload.encode("utf8"))
        script.flush()

        p = Popen(["ngless", script.name])
        p.communicate()

    if p.returncode:
        sys.stderr.write("ERROR: ngless failed with exit code {0}\n".format(p.returncode))
        sys.exit(p.returncode)


def main():
    args = parse_args()
    ngless(args)


if __name__ == "__main__":
    main()

# vim: ai sts=4 et sw=4
