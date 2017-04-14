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
                        help="FastQ file with reads to map (forward)")
    parser.add_argument("-i2", "--input-reverse",
                        help="FastQ file with reads to map (reverse) - if paired end")
    parser.add_argument("-s", "--input-singles",
                        help="FastQ file with reads to map (singles) - if paired end and unpaired reads exist")
    parser.add_argument("-o", "--output", required=True,
                        help="Output file/path for results")
    parser.add_argument("--debug", action="store_true",
                        help="Prints the payload before submitting to ngless")

    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-r", "--reference",
                       choices=["sacCer3", "ce10", "dm3", "gg4", "canFam2",
                                "rn4", "bosTau4", "mm10", "hg19"],
                       help="Map against a builtin reference")
    group.add_argument("-f", "--fasta",
                       help="Map against a given fasta file (will be indexed if index is not available)")

    args = parser.parse_args()

    if args.input_singles and not args.input_reverse:
        parser.error("--input-singles cannot be used without --input-reverse, use --input instead")

    if args.input_reverse:
        args.target = "paired"
    else:
        args.target = "fastq"

    return args


def prepare(args):
    # NOTE this needs to match the arguments in parse_args and the targets in payload
    # commas at the beginning of each option are used when that section has
    # parameters from other functions.
    options = {
        "input_opts": {
            "input": "'{input}'",
            "input_reverse": "'{input_reverse}'",
            "input_singles": "singles='{input_singles}'",
        },
        "write_opts": {
            "output": ", ofile='{output}'",
        },
        "target_name": {
            "target": "{target}",
        },
        "target_opts": {
            "reference": ", reference='{reference}'",
            "fasta": ", fafile='{fasta}'",
        },
    }

    ngl_options = ngl_prepare_options(args, options)

    payload_tpl = """\
ngless "0.0"
input = {target_name}({input_opts})
mapped = map(input{target_opts})
write(mapped{write_opts})
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
