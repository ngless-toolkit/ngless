===============================================
Ocean Metagenomics Assembly and Gene Prediction
===============================================

In this tutorial, we will analyse a small dataset of oceanic microbial
metagenomes.

.. note::
    This tutorial uses the full Ocean Microbial Reference Gene Catalog
    presented in `Structure and function of the global ocean microbiome
    <https://science.sciencemag.org/content/348/6237/1261359.long>`__ Sunagawa,
    Coelho, Chaffron, et al., Science, 2015


1. Download the toy dataset

First download all the tutorial data::

   ngless --download-demo ocean-short

We are reusing the same dataset as in the `Ocean profiling tutorial
<tutorial-ocean-metagenomics.html>`__. It may be a good idea to read steps 1-4
of that tutorial before starting this one.

2. Preliminary imports

To run ngless, we need write a script. We start with a few imports::

    ngless "1.4"


3. Preprocessing

First, we want to trim the reads based on quality::

    sample = 'SAMEA2621155.sampled'
    input = load_mocat_sample(sample)

    input = preprocess(input, keep_singles=False) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard

4. Assembly and gene prediction

This is now very simply two calls to the function `assemble
<Functions.html#assemble>`__ and `orf_find <Functions.html#orf_find>`__::

    contigs = assemble(input)
    write(contigs, ofile='contigs.fna')

    orfs = orf_find(contigs)
    write(contigs, ofile='orfs.fna')


Full script
-----------

::

    ngless "1.4"

    sample = 'SAMEA2621155.sampled'
    input = load_mocat_sample(sample)

    input = preprocess(input, keep_singles=False) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard

    contigs = assemble(input)
    write(contigs, ofile='contigs.fna')

    orfs = orf_find(contigs)
    write(contigs, ofile='orfs.fna')
