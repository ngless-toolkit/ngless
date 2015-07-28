.. _Language:

========
Language
========

This is a semi-formal definition of the NGLess language.

Basics
------

Tokenization follows the standard C-family rules. A word is anything that
matches ``[A-Za-z_]``. The language is case-sensitive. All files are UTF-8.

Script-style (# to EOL), C-style (/* to \*/) and C++-style (// to EOL) comments
are all recognised.

Both LF and CRLF are accepted as line endings (Unix-style LF is preferred).

A semicolon (;) can be used as an alternative to a new line. Any spaces (and
only space characters) following a semi-colon are ignored. *This feature is
intended for inline scripts at the command line (passed with the ``-e``
option), its use for scripts is heavily discouraged and may trigger an error in
the future.*

Strings are denoted with single or double quotes and standard backslashed
escapes apply (\\n for newline, ...).

A symbol is denoted as a token surrounded by curly braces (e.g., ``{symbol}``
or ``{gene}``).

Integers are specified as decimals ``[0-9]+`` or as hexadecimals
``0x[0-9a-fA-F]+``.

Booleans are denoted as ``true`` or ``false``.

Version declaration
-------------------

The first line of an NGLess file should be a version declaration::
 
   ngless "0.0"

Future versions of ngless will increase the string value. Also serves as a
magic constant for other tools.

Module Import Statments
-----------------------

Following the version statement, it is possible to add module import
statements, for example::

    import "batch" version "1.0"

This statement specifies that the ``batch`` module, version ``1.0`` should be used in
this script. Module versions are independent of ngless versions.

Use statements are not legal except as the first block.

Comments
--------

Single line comments start with ``#`` or ``//`` and run to the end of the
line::

    i = 10 // Assign ten to variable 'i'

Multi-line comments start with ``/*`` and end with ``*/``. As in C, they cannot
be nested. For example::

    /* This is a very long comment
     * It goes on and on..
     */

Data types
----------

NGless supports the following basic types:

- String
- Integer
- Bool
- Symbol
- Filename
- Shortread
- Shortreadset
- Mappedread
- Mappedreadset

In addition, it supports the composite type List of X where X is a basic type.
Lists are built with square brackets (e.g., [1,2,3]). All elements of a list
must have the same data type.

String
~~~~~~

A string can start with either a quote ``(U+0022, ")`` or a single quote
``(U+0027,')`` or and end with the same character. They can contain any number
of characters.

Special sequences start with a **\\\\**. Standard backslashed escapes can be
used as **LF** and ``CR`` (``\\n`` and ``\\r`` respectively), quotation marks
(``\\'``) or slash (``\\\``).

Integer
~~~~~~~

Integers are specified as decimals ``[0-9]+`` or as hexadecimals
``0x[0-9a-fA-F]+``. Use ``-`` to specify a negative number.

Boolean
~~~~~~~

The two boolean constants are ``True`` and ``False`` (which can also be written
``true`` or ``false``).

Symbol
~~~~~~

A symbol is denoted as a token surrounded by curly braces (e.g.. ``{symbol}``
or ``{gene}``).

Blocks are defined in multiples of 4 spaces. Tab characters are not allowed.

Variables
---------

NGless is a statically typed language and variables are typed. Types are
automatically inferred from context.

Assignment is performed with ``=`` operator::

    variable = value

A variable that is all upper-case is a constant and can only be assigned to
once.



Operators
---------

Unary
~~~~~
The operator **(-)** returns the symmetric of its integer argument.

The operator **len** returns the length of a ShortRead.

The operator ``not`` negates its boolean argument

Binary
~~~~~~

All operators can only be applied to integers. The operators described are available::

  + - < > >= <= == !=

Indexing
~~~~~~~~

Can be used to access only one element or a range of elements in a ShortRead. To access one element, 
is required an identifier followed by an expression between brackets. (e.g, x[10]).

To obtain a range, is required an identifier and two expressions separated by a
':' and between brackets. Example: 

+----------+--------------------------------------------------------+
| x[:]     | returns from position 0 until length of variable x     |
+----------+--------------------------------------------------------+
| x[10:]   | returns from position 10 util length of variable x     |
+----------+--------------------------------------------------------+
| x[:10]   | returns from position 0 until 10                       |
+----------+--------------------------------------------------------+

Conditionals
------------

Conditionals work as in Python. For example::

    if 5 > 10:
       val = 10
    else:
       val = 20


Functions
---------

Functions are called with parentheses::
  
  result = f(arg, arg1=2)

Functions have a single positional parameter, all other must be given by name::

    unique(reads, max_copies=2)

The exception is constructs which take a block: they take a single positional
parameter and a block. The block is passed using the using keyword: ::
  
  preprocess(reads) using |read|:
    block
    ...
    
There is no possibility of defining new functions. Only the built-in functions
are available.

Pure functions
~~~~~~~~~~~~~~

The following functions are pure functions:

- unique
- substrim
- map
- count
- as_reads
- select

The result of calling a pure function **must** be assigned to a variable or an
error is raised.

In the first version, there is no possibility of defining new functions. Only
the builtin functions are available.

Auto-comprehension
------------------

A function of type ``A -> * -> B`` can be automatically used as ``[A] -> * ->
[B]``::

    in1,in2 = fastq(["in1.fq", "in2.fq"])

This allows for a pipeline which runs in parallel over many input filenames.
