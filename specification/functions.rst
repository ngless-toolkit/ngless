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


unique
------

``unique:: ReadSet -> ReadSet``

Given a set of reads, returns another which only retains a set number of copies
of each read (if there are any duplicates).

Takes a ``max_copies`` parameter.

map
---

``map:: ReadSet -> MappedSet``

Maps to a reference.

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

