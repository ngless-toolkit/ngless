# Advanced options

## Subsample mode

Subsample mode means *throw away 99% of the data*. This allows you to quickly
check whether your pipeline works as expected:

    ngless --subsamplemode script.ngl

will run `script.ngl` in subsample mode, which will probably run much faster
than the full pipeline, allowing to quickly spot check any issues.

Subsample mode also changes all your `write()` so that the output files include
the `subsample` extension. That is, a call such as

    write(output, ofile='results.txt')

will automatically get rewritten to

    write(output, ofile='results.txt.subsample')

The goal is that you do not confuse subsampled results with the real thing.

