=======================================
Ocean Metagenomics Functional Profiling
=======================================

.. note::
    If you are starting out with NGLess for metagenomics profiling, consider
    using the predefined pipeline collection, `NG-meta-profiler
    <ng-meta-profiler.html>`__. This tutorial is based on deconstructing a
    pipeline very similar to those.


In this tutorial, we will analyse a small dataset of oceanic microbial
metagenomes.

.. note::
    This tutorial uses the full Ocean Microbial Reference Gene Catalog
    presented in `Structure and function of the global ocean microbiome
    <http://science.sciencemag.org/content/348/6237/1261359.long>`__ Sunagawa,
    Coelho, Chaffron, et al., Science, 2015

    This catalog contains ca. 40 million genes and requires **56GiB** of RAM


1. Download the toy dataset

First download all the tutorial data::

   ngless --download-demo ocean-short

This will `download
<http://vm-lux.embl.de/~coelho/ngless-data/Demos/ocean-short.tar.gz>`__ and
expand the data to a directory called ``ocean-short``.

This is a toy dataset. It is based on real data, but the samples were trimmed
so that they contains only 250k paired-end reads.

The dataset is organized in classical MOCAT style. Ngless does not require this
structure, but this tutorial also demonstrates how to upgrade from your
existing MOCAT-based projects.::

    $ find
    ./SAMEA2621229.sampled
    ./SAMEA2621229.sampled/ERR594355_2.short.fq.gz
    ./SAMEA2621229.sampled/ERR594355_1.short.fq.gz
    ./SAMEA2621155.sampled
    ./SAMEA2621155.sampled/ERR599133_1.short.fq.gz
    ./SAMEA2621155.sampled/ERR599133_2.short.fq.gz
    ./SAMEA2621033.sampled
    ./SAMEA2621033.sampled/ERR594391_2.short.fq.gz
    ./SAMEA2621033.sampled/ERR594391_1.short.fq.gz
    ./tara.demo.short
    ./process.ngl


The whole script we will be using is there as well (``process.ngl``), so you
can immediately run it with::

    ngless process.ngl

The rest of this tutorial is an explanation of the steps in this script.

2. Preliminary imports

To run ngless, we need write a script. We start with a few imports::

    ngless "0.7"
    import "parallel" version "0.6"
    import "mocat" version "0.0"
    import "omrgc" version "0.0"

These will all be used in the tutorial.

3. Parallelization

We are going to process each sample separately. For this, we use the ``lock1``
function from the `parallel <stdlib.html#parallel-module>`__ module (which we
imported before)::

    samples = readlines('tara.demo.short')
    sample = lock1(samples)

The ``readlines`` function reads a file and returns all lines. In this case, we
are reading the ``tara.demo.short`` file, which contains the three samples
(``SAMEA2621229.sampled``, ``SAMEA2621155.sampled``, and
``SAMEA2621033.sampled``).

``lock1()`` is a slightly more complex function. It takes a list and *locks one
of the elements* and returns it. It always chooses an element which has not
been locked before, so you each time you run _ngless_, you will get a different
sample.


4. Preprocessing

First, we load the data (the FastQ files)::

    input = load_mocat_sample(sample)

And, now, we preprocess the data::

    input = preprocess(input, keep_singles=False) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard


5. Profiling using the OM-RGC

After preprocessing, we map the reads to the ocean microbial reference gene
catalog::

    mapped = map(input, reference='omrgc', mode_all=True)

The line above is the reason we needed to import the ``omrgc`` module: it made
the ``omrgc`` reference available.

::

    mapped = select(mapped, keep_if=[{mapped}, {unique}])

Now, we need to ``count`` the results. This function takes the result of the
above and aggregates it different ways. In this case, we want to aggregate by
KEGG KOs, and eggNOG OGs::

    counts = count(mapped,
                features=['KEGG_ko', 'eggNOG_OG'],
                normalization={scaled})

7. Aggregate the results

We have done all this computation, now we need to save it somewhere. We will
use the ``collect()`` function to aggregate across all the samples processed::

    collect(counts
            current=sample,
            allneeded=samples,
            ofile='omgc.profiles.txt')

8. Run it!

This is our script. We save it to a file (``process.ngl`` in this example) and
run it from the command line::

    $ ngless process.ngl

Note that we need a large amount (ca. 64GB) of RAM memory to be able to use the
OM-RGC. **You also need to run it once for each sample.** However, this can be
done in parallel, taking advantage of high performance computing clusters.


Full script
-----------

Here is the full script::

    ngless "0.8"
    import "parallel" version "0.0"
    import "mocat" version "0.0"
    import "omrgc" version "0.0"


    samples = readlines('tara.demo.short')
    sample = lock1(samples)
    input = load_mocat_sample(sample)

    input = preprocess(input, keep_singles=False) using |read|:
        read = substrim(read, min_quality=25)
        if len(read) < 45:
            discard

    mapped = map(input, reference='omrgc', mode_all=True)
    mapped = select(mapped, keep_if=[{mapped}, {unique}])
    collect(
            count(mapped,
                    features=['KEGG_ko', 'eggNOG_OG'],
                    normalization={scaled}),
            current=sample,
            allneeded=samples,
            ofile='omgc.profile.txt')


