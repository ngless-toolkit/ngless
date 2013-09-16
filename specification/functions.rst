=========
Functions
=========

fastq
-----

::

    fastq:: Str -> ReadSet
    inputdata = fastq('input.fq')

Argument type: ``String``

Keyword arguments:
- ``type``: symbol, one of ``{auto}`` (default) or ``{solexa}``
- ``compression``: symbol, one of ``{auto}`` (default), ``{none}``, ``{gzip}``,
or ``{bzip2}``

Block: ``None``

Returns: ``ReadSet``

This function reads one (or more) FastQ files and performs quality control.


preprocess
----------

``preprocess:: ReadSet -> Block -> ReadSet``

This function executes the block for each read in the input. Unless the read is
discarded, it is transfered (after transformations) to the output.

This function performs quality control on its output.



unique
------

``unique:: ReadSet -> ReadSet``

Given a set of reads, returns another which only retains a set number of copies
of each read (if there are any duplicates).

Takes a ``max_copies`` parameter.

map
---

``map:: ReadSet -> MappedSet``

Parameters
~~~~~~~~~~

reference
    A string. It could be a path to a fasta file or a reference known to
    ngless.

Maps to a reference. In the future, this function should also take parameters
to specify the mapping parameters.

annotate
--------

``annotate :: MappedSet -> CountMap``

Parameters
~~~~~~~~~~

features
    A set of symbols.

write & print
-------------

``write:: * -> ()``

``print:: * -> ()``

Writes to a file (or, for ``print`` to standard output). Otherwise, these are
identical functions.

Example::

    write(counts,
            file="file.csv",
            format={csv})

