#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

from subprocess import Popen
import sys
import tempfile
from ngless.wrap import ngl_prepare_options, ngl_prepare_payload, ngl_as_list

try:
    import argparse
except ImportError:
    print("argparse not found. Please install argparse with 'pip install argparse'")
    sys.exit(1)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input", required=True,
                        help="SAM/BAM/CRAM file filter")
    parser.add_argument("-o", "--output", required=True,
                        help="Output file/path for results")
    parser.add_argument("-a", "--action", required=True,
                        choices=["keep_if", "drop_if"],
                        help="Whether to keep or drop when condition are met")
    parser.add_argument("-c", "--conditions", required=True, nargs="+",
                        choices=["mapped", "unmapped", "unique"],
                        help="One or more conditions to filter on")
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
        "action_opts": {
            "action": "{action}",
        },
        "cond_opts": {
            "conditions": ngl_as_list("{conditions}", wrap_with="{}"),
        },
    }

    ngl_options = ngl_prepare_options(args, options)

    payload_tpl = """\
ngless "0.0"
mapped = samfile({input_opts})
selected = select(mapped, {action_opts}={cond_opts})
write(selected{write_opts})
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
