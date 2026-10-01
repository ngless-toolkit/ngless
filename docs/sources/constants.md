# NGLess Constants

In NGLess, any variable whose name consists only of uppercase letters (e.g.,
`OUTPUT`) is a constant, i.e., can only be assigned to once. Note that a name
containing any other character, such as an underscore or a digit (e.g.,
`OUTPUT_DIR`), is a normal variable. In addition, there are builtin constants defined by NGLess.

## Built in constants

- ARGV

This is string array which contains the arguments passed to the script

- STDIN

Use in place of a filename to read from standard input

- STDOUT

Use in place of a filename to write to standard output

For example:

    ngless '1.6'

    input = samfile(STDIN)
    input = select(input) using |mr|:
        if mr.flag({mapped}):
            discard
    write(input, ofile=STDOUT, format={bam})


This file reads a sam stream from stdin, filters it (using the `select` call)
and writes to standard output in `bam` format.

