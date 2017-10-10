# Reproducible Computation With NGLess

NGLess has several builtin features to make it easier to achieve reproducible
research.

## Annotate results with input script

The `write()` function call supports the argument `auto_comments` which will
add (as comments) meta information to the output. In particular, you can use
the `{script}` auto comment to add the script to your output. For example:

    ngless '0.0'
    mapped = samfile('input.bam')

    counted = count(mapped, features=['seqname'])
    write(counted,
            ofile='output.txt',
            auto_comments=[{script}]) # <<<< ADD SCRIPT

This will add the script to your output. Thus, it will be easy to see how the
output was generated.

You can also use `{date}`, which will output a string with the date in which
the script was run (note that the result is no longer reproducible at the Byte
level as each run will contain a different date/time). Finally, the `comment`
argument allows for any free text string:

    write(counted,
            ofile="output.txt",
            comment="For my awesome Science publication",
            auto_comments=[{script}])

The `collect()` function also support the same arguments.

