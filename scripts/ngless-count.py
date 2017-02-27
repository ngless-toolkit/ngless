#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

from subprocess import Popen
import sys
import tempfile

try:
    import argparse
except ImportError:
    print("argparse not found. Please install argparse with 'pip install argparse'")
    sys.exit(1)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input", required=True,
                        help="SAM/BAM/CRAM file to count reads on")
    parser.add_argument("-o", "--output", required=True,
                        help="Output file/path for results")
    parser.add_argument("-m", "--multiple",
                        choices=["dist1", "all1", "1overN", "unique_only"],
                        help="Output file/path for results")
    parser.add_argument("--debug", action="store_true",
                        help="Prints the payload before submitting to ngless")

    return parser.parse_args()


def prepare_options(args):
    # NOTE this needs to match the arguments in parse_args and the targets in payload
    # commas at the beginning of each option are used when that section has
    # parameters from other functions.
    options = {
        "sam_opts": {
            "input": "'{input}'",
        },
        "count_opts": {
            "multiple": ", multiple={{{multiple}}}",
        },
        "write_opts": {
            "output": ", ofile='{output}'",
        },
    }

    parsed_dict = vars(args)

    result = dict()

    for category in options:
        for opt in options[category]:
            if category not in result:
                result[category] = []

            if parsed_dict[opt] is not None:
                result[category].append(options[category][opt])

    # Format list of options to string by joining options with commas
    for category in result.keys():
        result[category] = ", ".join(result[category])

    # Ensure all categories exist in the final dict even if empty
    for category in options:
        if category not in result:
            result[category] = ''

    if args.debug:
        from pprint import pprint
        print(">>> Options processed internally")
        print(args)
        pprint(result)
        print(">>> End of internal options")

    return result


def prepare_payload(args):
    payload_tpl = """\
ngless "0.0"
write(count(samfile({sam_opts}){count_opts}){write_opts})\
""".format(**prepare_options(args))

    payload = payload_tpl.format(**vars(args))

    if args.debug:
        print(">>> NGLess will be called with the following script:")
        print(payload)
        print(">>> End of script")

    return payload


def ngless(args):
    payload = prepare_payload(args)

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
