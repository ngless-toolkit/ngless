NGLess: NGS Processing with Less Work
=====================================

NGLess is a domain-specific language for NGS (next-generation sequencing
data) processing.

**Note**: This is *pre-release* software, currently available as a
preview only. Please `get in touch <mailto:coelho@embl.de>`__ if you
want to use it in your work.For questions, you can also use the `ngless
mailing list <https://groups.google.com/forum/#!forum/ngless>`__.

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

Basic features
--------------

-  preprocessing and quality control of FastQ files
-  mapping to a reference genome (implemented through
   `bwa <http://bio-bwa.sourceforge.net/>`__)
-  annotation and summarization of the alignments using reference gene
   annotations

Ngless has builtin support for some model organisms:

1. Homo sapiens (hg19)
2. Mus Muscullus (mm10)
3. Rattus norvegicus (rn4)
4. Bos taurus (bosTau4)
5. Canis familiaris (canFam2)
6. Drosophila melanogaster (dm3)
7. Caenorhabditis elegans (ce10)
8. Saccharomyces cerevisiae (sacCer3)

and the standard library includes support for `mOTUs <motus.html>`__.

Traditional Unix command line usage
-----------------------------------

``ngless`` can be used as a traditional command line transformer
utility, using the ``-e`` argument to pass an inline script on the
command line.

The ``-p`` (or ``--print-last``) argument tells ngless to output the
value of the last expression to ``stdout``.

Converting a SAM file to a FASTQ file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Extract file reads from a SAM (or BAM) file:

::

    $ ngless -pe 'as_reads(samfile("file.sam"))' > file.fq

This is equivalent to the full script:

::

    ngless "0.0" # <- version declaration, optional on the command line
    samcontents = samfile("file.sam") # <- load a SAM/BAM file
    reads = as_reads(samcontents) # <- just get the reads (w quality scores)
    write(reads, ofname=STDOUT) # <- write them to STDOUT (default format: FASTQ)

This only works if the data in the samfile is single ended as we pipe
out a single FQ file. Otherwise, you can always do:

::

    ngless "0.0"
    write(as_read(samfile("file.sam")),
            ofile="output.fq")

which will write 3 files: ``output.1.fq``, ``output.2.fq``, and
``output.singles.fq`` (the first two for the paired-end reads and the
last one for reads without a mate).

Getting aligned reads from a SAM file as FASTQ file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Building on the previous example. We can add a ``select()`` call to only
output unmapped reads:

::

    $ ngless -pe 'as_reads(select(samfile("file.sam"), keep_if=[{mapped}]))' > file.fq

This is equivalent to the full script:

::

    ngless "0.0" # <- version declaration, optional on the command line
    samcontents = samfile("file.sam") # <- load a SAM/BAM file
    samcontents = select(samcontents, keep_if=[{mapped}]) # <- select only *mapped* reads
    reads = as_reads(samcontents) # <- just get the reads (w quality scores)
    write(reads, ofname=STDOUT) # <- write them to STDOUT (default format: FASTQ)

Reading from STDIN
~~~~~~~~~~~~~~~~~~

For a true Unix-like utility, the input should be read from standard
input. This can be achieved with the special file ``STDIN``. So the
previous example now reads

::

    $ cat file.sam | ngless -pe 'as_reads(select(samfile(STDIN), keep_if=[{mapped}]))' > file.fq

Obviously, this example would more interesting if the input were to come
from another programme (not just ``cat``).

`Full documentation <http://ngless.readthedocs.org/en/latest/>`__

`Frequently Asked Questions
(FAQ) <http://ngless.readthedocs.org/en/latest/faq.html>`__

Building and installing
-----------------------

Again, please note that this is pre-release software. Thus, we do not
provide any easy to install (pre-built) packages at the moment, but they
will be provided once the software is released. However, any comments
(including bug and build reports), are more than welcome.

`stack <http://docs.haskellstack.org/en/stable/README/>`__ is highly
recommended. Install it and running ``stack build`` should (1) download
all dependencies with the correct versions and (2) build ngless. It will
perform this task in its own sandbox so it will not interfere with any
other work.

Authors
-------

-  `Luis Pedro Coelho <http://luispedro.org>`__ (email: coelho@embl.de)
   (on twitter: [@luispedrocoelho](https://twitter.com/luispedrocoelho))
-  Paulo Monteiro
-  `Ana Teresa Freitas <http://web.tecnico.ulisboa.pt/ana.freitas/>`__


.. toctree::
   :maxdepth: 2

   introduction
   install
   tutorial-ocean-metagenomics
   tutorial-gut-metagenomics
   tutorial
   command-line-wrappers
   one-liners
   preprocess
   Functions
   methods
   stdlib
   modules
   constants
   Organisms
   motus
   configuration
   reproducible
   faq
   advanced
   Language
