# Common Workflow Language Integrations

## Simple operations

Simple NGLess operations can be performed through the [command line
wrappers](command-line-wrappers.html), all of which have a CWL tool
description.

## Automatic CWL export of NGLess scripts

An NGLess script that conforms to certain rules can be exported as a CWL tool
using the `--export-cwl` option:

    ngless script.ngl --export-cwl=tool.cwl

The rules are simple: the script must use `ARGV` for its inputs and outputs.
For example, this is a conforming script:


    ngless "0.8"

    mapped = samfile(ARGV[1])

    mapped = select(mapped, drop_if=[{mapped}])

    write(mapped,
            ofile=ARGV[2])

The resulting tool will take two arguments, specifying its input and output.
