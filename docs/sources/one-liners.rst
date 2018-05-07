=================
NGLess one liners
=================

``ngless`` can be used as a traditional command line transformer utility, using
the ``-e`` argument to pass an inline script on the command line.

The ``-p`` (or ``--print-last``) argument tells ngless to output the value of
the last expression to *stdout*.

Converting a SAM file to a FASTQ file
-------------------------------------

::

    $ ngless -pe 'as_reads(samfile("file.sam"))' > file.fq

This is equivalent to the full script::

    ngless "0.8" # <- version declaration, optional on the command line
    samcontents = samfile("file.sam") # <- load a SAM/BAM file
    reads = as_reads(samcontents) # <- just get the reads (w quality scores)
    write(reads, ofname=STDOUT) # <- write them to STDOUT (default format: FASTQ)

This only works if the data in the samfile is single ended as we pipe out a
single FQ file. Otherwise, you can always do::

    ngless "0.8"
    write(as_read(samfile("file.sam")),
            ofile="output.fq")

which will write 3 files: ``output.1.fq``, ``output.2.fq``, and
``output.singles.fq`` (the first two for the paired-end reads and the last one
for reads without a mate).

Getting aligned reads from a SAM file as FASTQ file
---------------------------------------------------

Building on the previous example. We can add a ``select()`` call to only output
unmapped reads::

    $ ngless -pe 'as_reads(select(samfile("file.sam"), keep_if=[{mapped}]))' > file.fq

This is equivalent to the full script::

    ngless "0.8" # <- version declaration, optional on the command line
    samcontents = samfile("file.sam") # <- load a SAM/BAM file
    samcontents = select(samcontents, keep_if=[{mapped}]) # <- select only *mapped* reads
    reads = as_reads(samcontents) # <- just get the reads (w quality scores)
    write(reads, ofname=STDOUT) # <- write them to STDOUT (default format: FASTQ)

Reading from STDIN
------------------

For a true Unix-like utility, the input should be read from standard input.
This can be achieved with the special file ``STDIN``. So the previous example
now reads::

    $ cat file.sam | ngless -pe 'as_reads(select(samfile(STDIN), keep_if=[{mapped}]))' > file.fq

Obviously, this example would more interesting if the input were to come from another
programme (not just ``cat``).

