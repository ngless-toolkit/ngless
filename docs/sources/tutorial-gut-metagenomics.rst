
=======================================================
Human Gut Metagenomics Functional & Taxonomic Profiling
=======================================================

.. note::
    If you are starting out with NGLess for metagenomics profiling, consider
    using the predefined pipeline collection, `NG-meta-profiler
    <ng-meta-profiler.html>`__. This tutorial is based on deconstructing a
    pipeline very similar to those.

In this tutorial, we will analyse a small dataset of human gut microbial
metagenomes.


.. note::
    This tutorial is also available as a `slide presentation
    <https://ngless.embl.de/_static/gut-metagenomics-tutorial-presentation/gut_specI_tutorial.html>`__

1. Download the toy dataset

First download all the tutorial data::

   ngless --download-demo gut-short

This will `download
<https://ngless.embl.de/ressources/Demos/gut-short.tar.gz>`__ and
expand the data to a directory called ``gut-short``.

This is a toy dataset. It is based on `real data
<https://www.ebi.ac.uk/ena/data/view/PRJNA339914>`__, but the samples were
trimmed so that they contains only 250k paired-end reads.

The dataset is organized in classical MOCAT style, with one sample per
directory. NGLess does not require this structure, but this tutorial also
demonstrates how to upgrade from your existing MOCAT-based projects.::

    $ find
    ./igc.demo.short
    ./SAMN05615097.short
    ./SAMN05615097.short/SRR4052022.single.fq.gz
    ./SAMN05615097.short/SRR4052022.pair.2.fq.gz
    ./SAMN05615097.short/SRR4052022.pair.1.fq.gz
    ./SAMN05615096.short
    ./SAMN05615096.short/SRR4052021.pair.1.fq.gz
    ./SAMN05615096.short/SRR4052021.single.fq.gz
    ./SAMN05615096.short/SRR4052021.pair.2.fq.gz
    ./SAMN05615098.short
    ./SAMN05615098.short/SRR4052033.pair.2.fq.gz
    ./SAMN05615098.short/SRR4052033.pair.1.fq.gz
    ./SAMN05615098.short/SRR4052033.single.fq.gz
    ./process.ngl

The whole script we will be using is there as well (``process.ngl``), so you
can immediately run it with::

    ngless process.ngl

The rest of this tutorial is an explanation of the steps in this script.

2. Preliminary imports

To run ngless, we need write a script. We start with a few imports::

    ngless "1.0"
    import "parallel" version "0.6"
    import "mocat" version "0.0"
    import "motus" version "0.1"
    import "igc" version "0.0"

These will all be used in the tutorial.

3. Parallelization

We are going to process each sample separately. For this, we use the ``lock1``
function from the `parallel <stdlib.html#parallel-module>`__ module (which we
imported before)::


    samples = readlines('igc.demo.short')
    sample = lock1(samples)

The ``readlines`` function reads a file and returns all lines. In this case, we
are reading the ``tara.demo.short`` file, which contains the three samples
(``SAMEA2621229.sampled``, ``SAMEA2621155.sampled``, and
``SAMEA2621033.sampled``).

``lock1()`` is a slightly more complex function. It takes a list and *locks one
of the elements* and returns it. It always chooses an element which has not
been locked before, so you each time you run ``NGLess``, you will get a
different sample.

.. note::
   When you are using ``lock1()`` you will need to run ``NGLess`` multiple
   times. But you can run multiple instances in parallel.

3. Preprocessing

First, we load the data (the FastQ files)::

    input = load_mocat_sample(sample)

And, now, we preprocess the data::

    input = preprocess(input, keep_singles=False) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard


4. Filter against the human genome

We want to remove reads which map to the human genome, so we first map the
reads to the human genome::

    mapped = map(input, reference='hg19')

``hg19`` is a built-in reference and the genome will be automatically download
it the first time you use it. Now, we discard the matched reads::

    mapped = select(mapped) using |mr|:
        mr = mr.filter(min_match_size=45, min_identity_pc=90, action={unmatch})
        if mr.flag({mapped}):
            discard

The ``mapped`` object is a set of ``mappedreads`` (i.e., the same information
that is saved in a SAM/BAM file). we use the ``as_reads`` function to get back
to reads::

    input = as_reads(mapped)

Now, we will use the ``input`` object which has been filtered of human reads.

5. Profiling using the IGC

.. note::
    This section of the tutorial uses the `Integrated Gene Catalogue
    <http://www.nature.com/nbt/journal/v32/n8/full/nbt.2942.html>`__ and
    requires ca. **15GiB** of RAM. Skip to step 9 if your machine does not have
    this much memory.

After preprocessing, we map the reads to the integrated gene catalog::

    mapped = map(input, reference='igc', mode_all=True)

The line above is the reason we needed to import the ``igc`` module: it made
the ``igc`` reference available.

Now, we need to ``count`` the results. This function takes the result of the
above and aggregates it different ways. In this case, we want to aggregate by
KEGG KOs, and eggNOG OGs::

    counts = count(mapped,
                features=['KEGG_ko', 'eggNOG_OG'],
                normalization={scaled})

7. Aggregate the results

We have done all this computation, now we need to save it somewhere. We will
use the ``collect()`` function to aggregate across all the samples processed::

    collect(counts,
            current=sample,
            allneeded=samples,
            ofile='igc.profiles.txt')

9. Taxonomic profling using mOTUS

Map the samples against the ``motus`` reference (this reference comes with the
`motus module <motus.html>`__ we imported earlier)::

    mapped = map(input, reference='motus', mode_all=True)

Now call the built-in ``count`` function to summarize your reads at gene level::

    counted = count(mapped, features=['gene'], multiple={dist1})

To get the final taconomic profile, we call the ``motus`` function, which takes
the gene count table and performs the motus quantification. The result of this
call is another table, which we can concatenate with ``collect()``::

    motus_table = motus(counted)
    collect(motus_table,
            current=sample,
            allneeded=samples,
            ofile='motus-counts.txt')

10. Run it!

This is our script. We save it to a file (``process.ngl`` in this example) and
run it from the command line::

    $ ngless process.ngl

.. note:: **You need to run this script once for each sample**. However, this
    can be done in parallel, taking advantage of high performance computing
    clusters.


Full script
-----------

Here is the full script::

    ngless "1.0"
    import "parallel" version "0.6"
    import "mocat" version "0.0"
    import "motus" version "0.1"
    import "igc" version "0.0"

    samples = readlines('igc.demo.short')
    sample = lock1(samples)

    input = load_mocat_sample(sample)

    input = preprocess(input, keep_singles=False) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard

    mapped = map(input, reference='hg19')

    mapped = select(mapped) using |mr|:
        mr = mr.filter(min_match_size=45, min_identity_pc=90, action={unmatch})
        if mr.flag({mapped}):
            discard

    input = as_reads(mapped)


    mapped = map(input, reference='igc', mode_all=True)

    counts = count(mapped,
                features=['KEGG_ko', 'eggNOG_OG'],
                normalization={scaled})

    collect(counts,
            current=sample,
            allneeded=samples,
            ofile='igc.profiles.txt')

    mapped = map(input, reference='motus', mode_all=True)

    counted = count(mapped, features=['gene'], multiple={dist1})

    motus_table = motus(counted)
    collect(motus_table,
            current=sample,
            allneeded=samples,
            ofile='motus-counts.txt')
