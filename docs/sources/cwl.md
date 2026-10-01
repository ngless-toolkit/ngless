# Common Workflow Language Integrations

## Simple operations

Simple NGLess operations can be performed through the [command line
wrappers](command-line-wrappers.md), all of which have a CWL tool
description.

## Automatic CWL export of NGLess scripts

An NGLess script that conforms to certain rules can be exported as a CWL tool
using the `--export-cwl` option. This is an experimental feature, so it must be
enabled with `--experimental-features`:

    ngless --experimental-features --export-cwl=tool.cwl script.ngl

The rules are simple: the script must use `ARGV` for its inputs and outputs.
For example, this is a conforming script:


    ngless "1.6"

    mapped = samfile(ARGV[1])

    mapped = select(mapped, drop_if=[{mapped}])

    write(mapped,
            ofile=ARGV[2])

The resulting tool will take two arguments, specifying its input and output.
