# Advanced options

## Subsample mode

One of the distinguishing features of ngless is that it makes it easier to
identify and catch mistakes in the code and this is the purpose of subsample
mode (it is not to be used in production).

Subsample mode simply *throws away 99% of the data*. This allows you
to quickly check whether your pipeline works as expected and the output files
are as expected. For example:

    ngless --subsample script.ngl

will run `script.ngl` in subsample mode, which will probably run much faster
than the full pipeline, allowing to quickly spot any issues with your code. A
10 hour pipeline will finish in 5 minutes when running in subsample mode.

Subsample mode also changes all your `write()` so that the output
files include the `subsample` extension. That is, a call such as

    write(output, ofile='results.txt')

will automatically get rewritten to

    write(output, ofile='results.txt.subsample')

This ensures that you do not confuse subsampled results with the
real thing.

