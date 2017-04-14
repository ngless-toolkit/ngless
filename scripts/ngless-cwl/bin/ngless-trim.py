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
                        help="FastQ file with reads to trim")
    parser.add_argument("-o", "--output", required=True,
                        help="Output file/path for results")
    parser.add_argument("-m", "--method", required=True,
                        choices=["substrim", "endstrim"],
                        help="Which trimming method to use")
    parser.add_argument("-q", "--min-quality", type=int, required=True,
                        help="Minimum quality value")
    parser.add_argument("-d", "--discard", type=int, default=50,
                        help="Discard if shorted than")
    parser.add_argument("--debug", action="store_true",
                        help="Prints the payload before submitting to ngless")

    return parser.parse_args()


def prepare(args):
    # NOTE this needs to match the arguments in parse_args and the targets in payload
    # commas at the beginning of each option are used when that section has
    # parameters from other functions.
    options = {
        "fastq_opts": {
            "input": "'{input}'",
        },
        "write_opts": {
            "output": ", ofile='{output}'",
        },
        "method_name": {
            "method": "{method}",
        },
        "method_opts": {
            "min_quality": ", min_quality={min_quality}",
        },
        "discard_opts": {
            "discard": "{discard}",
        },
    }

    ngl_options = ngl_prepare_options(args, options)

    payload_tpl = """\
ngless "0.0"
input = fastq({fastq_opts})
preprocess(input) using |read|:
    read = {method_name}(read{method_opts})
    if len(read) < {discard_opts}:
        discard
write(input{write_opts})
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
