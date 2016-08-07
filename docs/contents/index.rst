NGLess: NGS Processing with Less Work
=====================================

NGLess is a domain-specific language for NGS (next-generation sequencing data)
processing.

**Note**: This is *pre-release* software, currently available as a preview
only. Please `get in touch <mailto:coelho@embl.de>`__ if you want to use it in
your work.For questions, you can also use the `ngless mailing list
<https://groups.google.com/forum/#!forum/ngless>`__.
 
NGLess is best illustrated by an example:

Example
-------

::

    ngless "0.0"
    input = fastq(['ctrl1.fq','ctrl2.fq','stim1.fq','stim2.fq'])
    preprocess(input) using |read|:
        read = read[5:]
        read = substrim(read, min_quality=26)
        if len(read) < 31:
            discard

    mapped = map(input,
                    reference='hg19')
    write(count(mapped, features=['gene']),
            ofile='gene_counts.csv',
            format={csv})

.. toctree::
   :maxdepth: 2

   introduction
   install
   tutorial
   one-liners
   preprocess
   Functions
   stdlib
   methods
   modules
   Organisms
   motus
   configuration
   faq
   advanced
   Language
