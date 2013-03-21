=====================================
NGLess: NGS Processing with Less Work
=====================================

This is a domain-specific language for NGS (next-generation sequencing data)
processing.

Example
-------

::

    ngless 0.0
    input = fastq(['ctrl1.fq','ctrl2.fq','stim1.fq','stim2.fq'])
    preprocess(input) using |read|:
        if read in contanminants:
            discard
        read = read[5:]
        read = substrim(read, min_quality=26)
        if len(read) < 31:
            discard

    mapped = map(input,
                    reference='hg19')
    annotated = annotate(mapped,
                    features=[{gene}])
    write(count(annotated, count={gene}),
            ofile='gene_counts.csv',
            format={csv})

Dependencies
------------

You need the following packages::

    cabal install cmdargs
    cabal install test-framework-th
    cabal install test-framework-quickcheck2
    cabal install test-framework-hunit

