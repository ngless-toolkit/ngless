# Reproducible Computation With NGLess

NGLess has several builtin features to make it easier to achieve reproducible
research.

All information that is needed to run a result is contained in the NGLess
script. There is no command line or configuration option which changes the
results: they only change the way in which the computation was run (what
information was printed on the console, where intermediate files were saved,
&c).

The version annotations that NGLess requires also enhance reproducibility while
allowing us to update NGLess going forward.

## Annotate results with input script

The `write()` function call supports the argument `auto_comments` which will
add (as comments) meta information to the output. In particular, you can use
the `{script}` auto comment to add the script to your output. For example:

    ngless '0.8'
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

Finally, you can use the magical `{hash}` auto comment:

    write(counted,
            ofile="output.txt",
            comment="For my awesome Science publication",
            auto_comments=[{script}, {hash}])

This will add a hash string to the output describing the computational path to
compute the result. This is smarter than a simple hash of the script as it does
not consider code that is not necessary to generate the script or elements such
as variable names (i.e., if you change the variable names, the hash will stay
the same as it is the same computational path).

The `collect()` function also support the same arguments.

