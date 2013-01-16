======
NGLess
======

Basics
------

Tokenization follows the standard C-family rules. A word is anything that
matches ``[A-Za-z_]``. The language is case-sensitive. All files are UTF-8.

Script-style (# to EOL), C-style (/* to \*/) and C++-style (// to EOL) comments
are all recognised.

Both LF and CRLF are accepted as line endings (LF is preferred).

Strings are denoted with single or double quotes and standard backslashed
escapes apply (\\n for newline, ...).

A symbol is denoted as a token preceded by ``:`` (e.g., ``:symbol`` or
``:gene``).

Language
--------

The first line of an NGLess file should be a version declaration::

    ngless 0

Functions
---------

Functions have a single positional parameter, all other *must be given by
name*.

The exception are constructs which take a block: they take a single positional
parameter and a block.


Auto-comprehension
------------------

A function of type ``A -> * -> B`` can be automatically used as ``[A] -> * ->
[B]``::

    in1,in2 = fastq(["in1.fq", "in2.fq"])

Builtin Functions
-----------------

``fastq:: Str -> ReadSet``
~~~~~~~~~~~~~~~~~~~~~~~~~~

Reads a FastQ file and computes all QC statistics.

``unique:: ReadSet -> ReadSet``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a set of reads, returns another which only retains a set number of copies
of each read (if there are any duplicates).

Takes a ``max_copies`` parameter.

``map:: ReadSet -> MappedSet``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Maps to a reference.

``write:: * -> ()`` and ``print:: * -> ()``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Writes to a file (or, for ``print`` to standard output). Otherwise, these are
identical functions.

Example::

    write(counts,
            file="file.csv",
            format=:csv)


