NGLess: NGS Processing with Less Work
=====================================

NGLess is a domain-specific language for NGS (next-generation sequencing
data) processing.

**Note**: NGLess is *beta* software: it should work, but you may run into some
rough edges.  Please `get in touch <mailto:coelho@embl.de>`__ if use it in your
work. For questions, you can also use the `ngless mailing list
<https://groups.google.com/forum/#!forum/ngless>`__.

NGLess is best illustrated by an example:

Example
-------

::

    ngless "0.5"
    input = paired('ctrl1.fq', 'ctrl2.fq', singles='ctrl-singles.fq')
    input = preprocess(input) using |read|:
        read = read[5:]
        read = substrim(read, min_quality=26)
        if len(read) < 31:
            discard

    mapped = map(input, reference='hg19')

    write(count(mapped, features=['gene']),
            ofile='gene_counts.csv',
            format={csv})

Building and installing
-----------------------

See the `install <install.html>`__ page for more information.

Basic functionality
-------------------

-  preprocessing and quality control of FastQ files
-  mapping to a reference genome (implemented through
   `bwa <http://bio-bwa.sourceforge.net/>`__ by default)
-  assembly of contigs
-  annotation and summarization of the alignments using reference gene
   annotations
-  `much more <Functions.html>`__

Ngless has builtin support for model organisms:

1. Homo sapiens (hg19)
2. Mus Muscullus (mm10)
3. Rattus norvegicus (rn4)
4. Bos taurus (bosTau4)
5. Canis familiaris (canFam2)
6. Drosophila melanogaster (dm3)
7. Caenorhabditis elegans (ce10)
8. Saccharomyces cerevisiae (sacCer3)

and the standard library includes support for `mOTUs <motus.html>`__,
metagenomics profiling of `marine samples <tutorial-ocean-metagenomics.html>`__
and `human gut microbiome samples <tutorial-gut-metagenomics.html>`__. We also
have `standard library modules <stdlib.html>`__ for helping users upgrading
from MOCAT or running many samples (we have used NGLess on projects with
>10,000 samples).

NGLess puts `a strong emphasis on reproducibility <reproducible.html>`__.

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

    ngless "0.7" # <- version declaration, optional on the command line
    samcontents = samfile("file.sam") # <- load a SAM/BAM file
    reads = as_reads(samcontents) # <- just get the reads (w quality scores)
    write(reads, ofname=STDOUT) # <- write them to STDOUT (default format: FASTQ)

This only works if the data in the samfile is single ended as we pipe
out a single FQ file. Otherwise, you can always do:

::

    ngless "0.7"
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

    ngless "0.7" # <- version declaration, optional on the command line
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

Authors
-------

-  `Luis Pedro Coelho <http://luispedro.org>`__ (email: coelho@embl.de)
   (on twitter: `@luispedrocoelho <https://twitter.com/luispedrocoelho>`__)
-  Paulo Monteiro
-  Renato Alves
-  `Ana Teresa Freitas <http://web.tecnico.ulisboa.pt/ana.freitas/>`__
-  Peer Bork


.. toctree::
   :hidden:
   :maxdepth: 2

   introduction
   install
   whatsnew
   tutorial-ocean-metagenomics
   tutorial-assembly-gp
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
   searchpath
   reproducible
   faq
   releases
   nglesspy
   cwl
   advanced
   Language
   Mapping
   software
