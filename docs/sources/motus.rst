===========================================
Taxonomic profiling using mOTUs with ngless
===========================================


You can use ngless to compute `mOTU profiles
<http://www.bork.embl.de/software/mOTU/>`__.

This requires the use of the (standard) motus module::

    ngless "0.8"
    import "motus" version "0.1"

This module (with the motus database) will be downloaded the first time you use
it.


You can use all the ngless functionality to load and `preprocess
<preprocess.htm>`__ your data::

    input = paired('input.1.fq.gz', 'input.2.fq.gz')

    files = preprocess(input, keep_singles=False) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard

Producing the motus tables is done in three steps.

1. Map the samples against the ``motus`` reference (this reference comes with
   the motus module we imported earlier)::

    mapped = map(files, reference='motus', mode_all=True)

2. call the built-in ``count`` function to summarize your reads at gene level,
   optionally adding ``include_minus1=true`` if you want to also obtain the
   fraction of unknown reads (``-1``). This is the default behavior from version ``0.6`` onwards::

    counted = count(mapped, features=['gene'], multiple={dist1})

3. call the ``motus`` function, which takes the gene count table and performs
   the motus quantification. The result of this call is another table, which
   can then be written out with the standard ``write`` call::

    table = motus(counted)
    write(table, ofile='motus-counts.txt')

This function is the only special function introduced by the ``motus`` module,
everything else is standard ngless.

You can see a full worked out example in the `examples/motus.ngl recipe
<https://github.com/ngless-toolkit/ngless/blob/master/examples/motus.ngl>`__

