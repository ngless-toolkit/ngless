.. _Language:

==============
Language
==============

Version declaration
-------------------
The first line of an NGLess file should be a version declaration:
::
 ngless "0.0"

Future versions of ngless will increase the string value. Also serves as a magic constant for other tools

Comments
-------------------

Explicative
~~~~~~~~~~~~

Start with **#** or **//** and end at the end of the line.
::
  i = 10 // This variable is used to explain Explicative comments

Operational
~~~~~~~~~~~~~

Start with **/*** and end with ***/**. Can't be nested.
::
  /*
     This comment is used to explain operational comments.
  */
  i = 10

Data types
-------------------

NGless supports the following basic types:

- string
- integer
- bool
- symbol
- filename
- shortread
- shortreadset
- mappedread
- mappedreadset

In addition, it supports the composite type List of X where X is a basic type. Lists are built
with square brackes (e.g., [1,2,3]). All elements in a list must have the same data type.

String
~~~~~~~~~

A string can start with either **ASCII 0x22 (")** or **ASCII 0x27 (')** and end with the same character. They can
contain any number of characters.

Special sequences start with a **\\**. Standard backslashed escapes can be used as **ASCII LF** and **CR** (**\\n** 
and **\\r** respectively), quotation marks (**\\'**) or slash (**\\\\**).

Integer
~~~~~~~~~
Integers are specified as decimals ``[0-9]+`` or as hexadecimals ``0x[0-9a-fA-F]+``. They are non negative, but 
can be negative through the use of the operator ``(-)``

Boolean
~~~~~~~~~
Booleans are denoted as the word \textbf{true} or \textbf{false}, with the first letter in upper or lower case.

Symbol
~~~~~~~~~~
A symbol is denoted as a token surrounded by curly braces (e.g., ``{symbol}`` or ``{gene}``).


Variables
-------------------
NGless is a statically typed language and variables are typed. Types are automatically inferred from context.

Assignment is performed with = operator:
::
 variable = value


Constants
~~~~~~~~~~~~~~~~~~~

A variable that is all upper-case is a constant and can only be assigned to once.

Functions
-------------------

Functions are called with parentheses:
::
  result = f(arg, arg1=2)

Functions have a single positional parameter, all other must be given by name:
::
  unique(reads, max_copies=2)

The exception are constructs which take a block: they take a single positional parameter and a block. The block is passed using the using keyword:
::
  preprocess(reads) using |read|:
    block
    ...
    
There is no possibility of defining new functions. Only the builtin functions are available.

Pure functions
~~~~~~~~~~~~~~~~~~~~

Functions that their result must be assigned to some variable are called pure functions. They are comprised of:

- unique
- substrim
- map
- count


Auto-comprehension
-------------------

A function of type ``A -> * -> B`` can be automatically used as ``[A] -> * ->
[B]``::

    in = fastq(["in1.fq", "in2.fq"])

This allows for a pipeline which runs in parallel over many input filenames.
