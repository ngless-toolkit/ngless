================
Advanced options
================

Subsample mode
~~~~~~~~~~~~~~

Subsample mode simply *throws away >90% of the data*: it keeps one in ten
input reads, up to a maximum of 25,000 reads per input file. This allows you
to quickly check whether your pipeline works as expected and the output files
have the expected format. Subsample mode should never be used in production.
To use it, pass the option ``--subsample`` on the command line::

    ngless --subsample script.ngl

will run ``script.ngl`` in subsample mode, which will probably run much faster
than the full pipeline, allowing to quickly spot any issues with your code. A
10 hour pipeline will finish in a few minutes (sometimes in just seconds) when
run in subsample mode.

.. note:: subsample mode is also a way to make sure that all indices exist. Any
    ``map()`` calls will check that the necessary indices are present: if a
    ``fafile`` argument is used, then the index will be built if necessary; if
    a ``reference`` argument is used, then the necessary datasets are
    downloaded if they have not previously been obtained.

Subsample mode also changes all your ``write()`` so that the output
files include the ``subsampled`` extension. That is, a call such as::

    write(output, ofile='results.txt')

will automatically get rewritten to::

    write(output, ofile='results.txt.subsampled')

When a paired-end ReadSet is written, the marker goes before the extension of
each file (e.g., ``output.pair.1.subsampled.fq.gz``). The output of
``collect()`` (from the `parallel module <stdlib.html#parallel-module>`__) gets
a ``.subsample`` suffix instead.

This ensures that you do not confuse subsampled results with the
real thing.

