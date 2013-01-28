======
NGLess
======

This is a semi-formal definition of the NGLess language.

Basics
------

Tokenization follows the standard C-family rules. A word is anything that
matches ``[A-Za-z_]``. The language is case-sensitive. All files are UTF-8.

Script-style (# to EOL), C-style (/* to \*/) and C++-style (// to EOL) comments
are all recognised.

Both LF and CRLF are accepted as line endings (Unix-style LF is preferred).

Strings are denoted with single or double quotes and standard backslashed
escapes apply (\\n for newline, ...).

A symbol is denoted as a token preceded by ``:`` (e.g., ``:symbol`` or
``:gene``).

Language
--------

The first line of an NGLess file should be a version declaration::

    ngless 0

This also serves as a *magic constant* for other tools

Blocking
--------

Blocks are defined in multiples of 4 spaces. Tab characters are not allowed.

Variables
---------

NGless is a statically typed language and variables are typed. Types are
automatically inferred from context.

Assignment is performed with ``=`` operator::

    variable = value

A variable that is all upper-case is a constant and can only be assigned once.

Functions
---------

Functions are called with parentheses::

    result = f(arg, arg1=2)

Functions have a single positional parameter, all other *must be given by
name*. The exception are constructs which take a block: they take a single
positional parameter and a block.

In the first version, there is no possibility of defining new functions. Only
the builtin functions are available.


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


