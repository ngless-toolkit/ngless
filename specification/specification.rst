.. sectnum::

====================
NGLess Specification
====================

.. role:: math(raw)
   :format: html latex
..

NGLess is a, imperative, domain specific language that infers variable
types from context and is presented in a precise manner in the following
document.

Basic Characteristics
=====================

Data Types
----------

There are 4 (four) basic data types that can be instantiated in NGLess:
Integers, chains of characters (String), Booleans (Bool) and Symbols.

-  **Integer** can hold any number, no matter how big.

-  **String** is a chain of Unicode characters.

-  **Bool** is an enumeration that can be either True or False.

-  **Symbol** represents a tag that may have different meaning in other
   contexts.

There are other data types, that are related to the biological context,
but they can not be directly created. They are:

-  **ReadSet** keeps information about a data set.

-  **ShortRead** is a read of a given data set.

-  **MappedReadSet** keeps information about the map.

-  **AnnotatedSet** stores annotation results.

-  **Void** keeps no information.

In addition, is supported the composite type **List of X** where **X**
is a basic type. All elements in **List** must have the same type. Lists
are built with square brackets (e.g., **[1,2,3]**, a list of three
integers).

The data types supported by each operator are indicated in the
expression definition (§ [sec:expr]).

Name manipulation
-----------------

The names (§ [sec:names]) correspond to constants and variables. In the
following topics the term entity is used to designate them.

Name space and identifiers visibility
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The name space is global and unique and for that reason a name used to
designate an entity, on a given context, cannot be used to designate any
other.

Identifiers are always visible. In particular, a redefined identifier on
a lower context will modify it at a upper context.

Validate variables
~~~~~~~~~~~~~~~~~~

The global entities (at the higher context), exist during the whole
execution of the program. The variables, local to a block, exist only
during his execution.

Auto comprehension
------------------

A function that receives a the data type **A** and returns **B** (A
:math:`→` \* :math:`→` B) can be automatically used
as receiving a **List of A** and returning a **List of B** (i.e [A]
:math:`→` \* :math:`→` [B]).

::

    in = fastq(["in1.fq", "in2.fq"])

In the previous example, function fastq receives a **List of String**
and returns a **List of ReadSet**.

Lexical Conventions
===================

For each of the lexical elements (tokens), is considered the biggest
sequence of characters existent that constitutes a valid lexical
element.

Line structure
--------------

A program is divided into logical lines and one instruction cannot
occupy more than one line, except if the instruction explicitly allows
for a line change, for example the if condition.

Physical lines
~~~~~~~~~~~~~~

A Physical line may have any size. The termination of a line (eol) does
not depend on the operating system and must equally work in UNIX, MAC OS
and Windows. The **line feed** (U+000A, **\\n**) and **carriage return**
(U+000D, **\\r**) are both accepted as Physical line (eol) terminators.

White characters
~~~~~~~~~~~~~~~~

Are considered white characters those that, even though are used to
separate lexical elements, do not represent one.

Is considered white characters the Unicode character space **U+0020**
(White Space, ’ ’). The character tabulation, **U+0009** (HT, \\t),
cannot be used in NGLess as a white character.

The mentioned character, even though is white, has special meaning if
appears at the beginning of a logical line. Similarly, the characters of
line change (eol) also have special meaning in indentation
(§ [sec:indentation]) and comments (§ [sec:comments]).

White lines
~~~~~~~~~~~

A logical line that only contains white characters and comments is
ignored, not being generated any lexical element neither changing the
indentation.

Indentation
~~~~~~~~~~~

As previously mentioned in § [sec:white], the character space is not
considered white when in the beginning of a logical line.

The total number of white spaces until the first non-white character
defines the level of indentation of the block. Each block must have the
same level of indentation and it should be a multiple of 4.

Even though the levels of indentation might be different, the meaning
can be the same:

3

::

    1. ngless "0.0"
    2. in = fastq('sample.fq')
    3. preprocess(in) using |read|:
    4.    read = read[3:]
    5.    if len(read) < 20:
    6.        discard
    7. m = map(in,reference='ce10')

a. Correct indentation.

::

    1. ngless "0.0"
    2. in = fastq('sample.fq')
    3. preprocess(in) using |read|:
    4.  read = read[3:]
    5.  if len(read) < 20:
    6.       discard
    7. m = map(in,reference='ce10')

b. Correct indentation.

::

    1. ngless "0.0"
    2. in = fastq('sample.fq')
    3.   preprocess(in) using |read|:
    4. read = read[3:]
    5. if len(read) < 20:
    6.     discard
    7. m = map(in,reference='ce10')

c. Incorrect indentation.

Examples **a)** and **b)** are correct has both have an increasing
indentation level and all instructions that are at the same level are
consistent with the context. At example **c)**, the instruction with the
**’preprocess’** function (Line 3) has two indentation errors:

-  Indentation level higher than the one at the current context.

-  The provided block has lower indentation level and should have
   higher.

Comments
--------

The comments work as separators of lexical elements. There are two kinds
of comments:

Single-line
    Start with **#** or **//** (as long as the sequence does not belong
    to a chain of characters) and end at the line termination.

Multi-line
    Start with **/\*** and terminates with **\*/** (if the sequence does
    not belong to a chain of characters). Cannot be nested.

Key Words
---------

The following words are reserved and do not constitute identifiers:

**if**, **else**, **ngless**, **len**, **discard**, **continue**,
**using**

Operators
---------

Are considered operators the following lexical elements:

**= + - \* [ ] != == <= < >= >**

Delimiters and Terminators
--------------------------

The following lexical elements are considered delimiters/terminators:

**, : ( )**

Identifiers (names)
-------------------

Are initiated by a letter (uppercase or lowercase) or by a ’\_’
(underscore). The first character can be followed by 0 (zero) or more
letters, digits and underscores. The number of characters that
constitute an identifier is unlimited and two names are distinct if
there is a transformation of uppercase to lowercase, or vice versa, of
at least one character.

Literals
--------

Are notations for constant values of data types provided by NGLess.

Integers
~~~~~~~~

An integer literal is a non rational number non negative (can be
negative by the application of the unary operator (-) to a positive
literal).

An integer literal in decimal is constituted by a sequence of 1 (one) or
more digits (from 0 to 9). An integer literal in hexadecimal starts with
the sequence **0x**, followed by one or more digits from 0 to 9, a to f
or A to F.

There is an unlimited size representation to a integer.

Chain of characters (string)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A string can start with either a single quote\ **(U+0027,”’)** or a
quote **(U+0022, ’"’)** and end with the same character. They can
contain any number of characters.

Special sequences are initiated by a \\. They can be represented by the
characters **LF** and **CR** (\\n and \\r respectively), quotation marks
(\\’) or slash (\\\\).

Boolean
~~~~~~~

A boolean literal can be represented as word **True** or **False**, with
the first letter in upper or lower case.

Symbol
~~~~~~

The representation is a token involved in curly brackets (**{}**).
(e.g., {CDS} or {gene}). Symbols have specific meaning to functions
(§ [sec:functions]) as they allow to parametrise them.

Complex data type format
------------------------

This data types result always from function invocations and cannot be
created directly.

ReadSet
~~~~~~~

A ReadSet is a file in fastQ format. It has 1 or more **ShortRead**.

ShortRead
~~~~~~~~~

A ShortRead is each read of a **ReadSet** with the following structure:

<read> ::= ‘@’<seqname>‘\\n’<seq>‘\\n’+‘\\n’<qual>‘\\n’

<seqname> ::= String

<seq> ::= String

<qual> ::= [!-~]+

The <qual> represents the quality of the read and has a range of values
from 33 to 126.

MappedReadSet
~~~~~~~~~~~~~

MappedReadSet follows the Sequence Alignment/Map(SAM) format. The SAM
Format allows to store sequence data in a series of tab delimited
columns.

The SAM file is divided into two sections, the header and the alignment.
The first contains information of the entire file and additional
alignment information. The latter contains the information for each
sequence about the alignment.

Each line in the alignment section has 11 mandatory fields. In the
following table is represented each field and his type.

+----------+---------+-----------+
| Column   | Field   | Type      |
+==========+=========+===========+
| 1        | QNAME   | String    |
+----------+---------+-----------+
| 2        | FLAG    | Integer   |
+----------+---------+-----------+
| 3        | RNAME   | String    |
+----------+---------+-----------+
| 4        | POS     | Integer   |
+----------+---------+-----------+
| 5        | MAPQ    | Integer   |
+----------+---------+-----------+
| 6        | CIGAR   | String    |
+----------+---------+-----------+
| 7        | RNEXT   | String    |
+----------+---------+-----------+
| 8        | PNEXT   | Integer   |
+----------+---------+-----------+
| 9        | TLEN    | Integer   |
+----------+---------+-----------+
| 10       | SEQ     | String    |
+----------+---------+-----------+
| 11       | QUAL    | String    |
+----------+---------+-----------+

AnnotatedSet
~~~~~~~~~~~~

A AnnotatedSet stores the result of annotating a given MappedReadSet. It
follows a tab delimited structure and his represented next:

+----------+------------+------------------+
| Column   | Field      | Type             |
+==========+============+==================+
| 1        | Id         | String           |
+----------+------------+------------------+
| 2        | Features   | List of Symbol   |
+----------+------------+------------------+
| 3        | Counts     | Integer          |
+----------+------------+------------------+
| 4        | Strand     | String           |
+----------+------------+------------------+

Grammar
=======

The language grammar can be resumed by the rules described next.
Consider that:

-  elements in fix type are called literals.

-  optional items are enclosed in square brackets (i.e [<item-x>]).

-  alternative elements are separated by a vertical bar (‘\|’).

-  items repeating 0 or more times are suffixed with an asterisk (’\*’).

-  items repeating 1 or more times are suffixed with a plus (’+’).

<script> ::= <version> <body>

<version> ::= ‘ngless’ <literal-string>

<body> ::= <instructions>

<block> ::= <indentation> <instructions>

<instructions> ::= \*<expr>

<expr> ::= ‘continue’ \| ‘discard’ <conditional> <iteration>
<assignment> <funccall> <inner-expr>

<inner-expr> ::= <binary-op> <base-expr> <index-expr> <list-expr>

<base-expr> ::= <pexpr> <literal> <unary-op> <variable>

<variable> ::= <word-req> \*<word-opt>

<word-req> ::= letter \| ‘\_’

<word-opt> ::= <word-req> \| digit

<pexpr> ::= ‘(’ <expr> ‘)’

<conditional> ::= ‘if’ <expr> ‘:’ <block> “[” ‘else’ ‘:’ <block> “]”

<iteration> ::= ‘preprocess’ ‘(’<expr>‘)’ ‘using’ ‘\|’ <variable> ‘\|’
‘:’ <block>

<assignment> ::= <variable> ‘=’ <expr>

<funccall> ::= <func-name> ‘(’ <expr> “\*”<opt-args> ‘)’

<opt-args> ::= ‘,’ <opt-arg>

<opt-arg> ::= <variable> ‘=’ <expr>

<list-expr> ::= ‘[’ ‘]’ \| ‘[’ <inner-expr> “\*”<inner-expr-opt> ‘]’

<inner-expr-opt> ::= ‘,’ <ìnner-expr>

<index-expr> ::= <base-expr> <index-one> <base-expr> <index-two>

<index-two> ::= ‘[’ “[” <expr> “]” ‘:’ “[” <expr> “]” ‘]’

<index-one> ::= ‘[’ <expr> ‘]’

<binary-op> ::= <base-expr> <bin-ops> <expr>

<unary-op> ::= <len-op> <unary-minus>

<unary-minus> ::= ‘-’ <base-expr>

<len-op> ::= ‘len’ <pexpr>

<indentation> ::= <literal-int>

<bin-ops> ::= ‘!=’ \| ‘==’ \| ‘<=’ \| ‘<’ \| ‘>=’ \| ‘>’ \| ‘+’ \| ‘\*’

<func-name> ::= ‘fastq’ \| ‘substrim’ \| ‘preprocess’ \| ‘map’ \|
‘count’ \| ‘unique’ \| ‘write’ \| ‘print’ \| ‘annotate’

<literal> ::= <literal-int> <literal-bool> <literal-string>
<literal-symbol>

<literal-string> ::= String

<literal-symbol> ::= Symbol

<literal-int> ::= Integer

<literal-bool> ::= Boolean

The precedence of binary and unary operators are described in detail at
section § [sec:expr]. Also, the values that the literals can take are
defined in § [sec:literals]

Left value
----------

The elements of an expression (operators) that can be used as a
left-value are individually identified in section § [sec:expr].

Script
------

Is designated by script the file that contain all the code to run on
NGLess. All scripts must be in **UTF-8** format.

Variables and constants
-----------------------

Initialization
~~~~~~~~~~~~~~

Is performed with a value that follows the operator **=** (“equal”):
integer (an Integer expression), string (a String expression), boolean
(a Bool expression) and a symbol (a Symbol expression). Examples:


+-----------+---------------------------+--------------+
| Integer   | :math:`\027f9`            | i = 2        |
+-----------+---------------------------+--------------+
| String    | :math:`\u27f9`            | s = ’hey’    |
+-----------+---------------------------+--------------+
| Boolean   | :math:`\u27f9`            | b = True     |
+-----------+---------------------------+--------------+
| Symbol    | :math:`\u27f9`            | s = {gene}   |
+-----------+---------------------------+--------------+

To associate a variable with an array of expressions, it’s required to
start with the **[** operator and terminate with **]**. The expression
should have the same type. Examples:

+-------------------+---------------------------+--------------------------+
| List of symbols   | :math:`\u27f9`            | ls = [ {gene}, {CDS} ]   |
+-------------------+---------------------------+--------------------------+
| List of strings   | :math:`\u27f9`            | ls = [ ‘fp1’, ‘fp2’ ]    |
+-------------------+---------------------------+--------------------------+


Constants
~~~~~~~~~

The language allows for the definition of constant identifiers,
preventing it of being used in operations that modify it’s value. All
characters in the identifier must be in upper case. Examples:

+--------------------+---------------------------+---------------+
| Constant integer   | :math:`\u27f9`            | CI = 2        |
+--------------------+---------------------------+---------------+
| Constant string    | :math:`\u27f9`            | CS = ’hey’    |
+--------------------+---------------------------+---------------+
| Constant boolean   | :math:`\u27f9`            | CB = True     |
+--------------------+---------------------------+---------------+
| Constant symbol    | :math:`\u27f9`            | CS = {gene}   |
+--------------------+---------------------------+---------------+

Functions
=========

A function allows to execute predefined code to a given set of
parameters, that are provided by argument.

With NGLess is not possible to define new functions. Nonetheless, a big
variety of functions is provided and are described in § [sec:functions].

Invocation
----------

A function can only be invoked by the use of an identifier that refers
to one of the provided functions. After the identifier, the delimiter
**(** is opened to refer to the start of the arguments and ended with
the delimiter **)**.

Functions have a single positional parameter and all other must be
provided by name. Example:

::

        result = f(arg, arg1=2)

The argument **arg** can be any data type (§ [sec:datatypes]) as for
**arg1** it is an expression that evaluates either to a string, integer,
bool or a symbol. The variable **result** will store a new data type
that is consequence of executing the function **f**.

In most cases, all arguments are passed by value and no modification is
made by the execution of a given function. In particular, there is one
exception (preprocess) and is detailed in § [sec:specin].

Parametrize functions
---------------------

To parametrize functions, arguments must be passed by name. They are
optional as all functions have default values, in case one is not
provided. It works as a variable assignment but reflects only to the
function.

It is not possible to pass arguments by name to a function that has no
use to them. The names and possible values must be followed and are
detailed in § [sec:functions].

Pure functions
--------------

The result of calling a pure function must be assigned to another
variable. At § [sec:functions] is indicated which function are pure.

Special invocation
------------------

Instead of multiple parameters, there is the case where a function takes
a single positional parameter and a block. The block is a closure that
is passed to the function and the parameter is passed by reference,
which means that the variable provided will be modified.

The block is passed with the **using** keyword. All instructions in the
block must have the same indentation, using **white space (U+0020, ’
’)**. Example:

::

    f(all) using |i|:
        block

It works as a for each, the contents of the variable **all** is
traversed and kept in the variable **i** for each iteration.

At the end the result of the function is assigned to the variable passed
as argument, in this example **all**. The flow of the execution must
continue even if the result is an empty data set.

Instructions
============

All instructions are executed in sequence.

Conditional instruction
-----------------------

If the expression evaluates to a **true** boolean then the block that
follows the operator ’:’ is executed.

If the expression evaluates to a **false** boolean and is present the
reserved word **else** and delimiter ’:’ , the else block is executed.
If evaluates to **false** and the reserved word **else** is not present,
nothing happens.

Case both the reserved word **if** and **else** are present, exactly one
of the two blocks will be executed.

Iteration instruction
---------------------

It is only possible to iterate the data type **ReadSet**. A ReadSet has
zero or more **ShortRead**.

::

    preprocess(reads) using |read|:
        block

The variable **read** (ShortRead) contains each read of the variable
**reads** (ReadSet).

The variable passed to the ’preprocess’, in this case **reads**, is
passed by reference and so every modification to the variable **read**
in the block modifies the variable **reads**.

Discard instruction
-------------------

Discard instruction can only be used inside an iteration instruction
(§ [sec:iter]).

Indicated by the reserved word **discard** (when executed, is the last
instruction in the block) it removes the current ShortRead (being
iterated) from the ReadSet and jumps for the next iteration.

Continue instruction
--------------------

Discard instruction can only be used inside an iteration instruction
(§ [sec:iter]).

Indicated by the reserved word **continue** (when executed, is the last
instruction in the block) it stops executing the block to an ShortRead
(being iterated) from the ReadSet and jumps to the next iteration.

Expressions
===========

The expression are always evaluated from the left to the right,
independent of the operator associativity.

The operators precedence is the same when in the same section, knowing
that following sections represent less priority.

The following table is a resume of the possible operators and is grouped
by decreasing precedence.

+------------------+-------------+-------------------+
| primary          | () []       | non associative   |
+------------------+-------------+-------------------+
| unary            | - len       | non associative   |
+------------------+-------------+-------------------+
| multiplicative   | \*          | left to right     |
+------------------+-------------+-------------------+
| aditive          | + -         | left to right     |
+------------------+-------------+-------------------+
| comparative      | < > >= <=   | left to right     |
+------------------+-------------+-------------------+
| equality         | == !=       | left to right     |
+------------------+-------------+-------------------+
| atribution       | =           | right ro left     |
+------------------+-------------+-------------------+

For example the following expressions would result in different values,
due to precedence:

-  5 + 5 \* 2 = 15

-  ((5 + 5) \* 2) = 20

Primary expressions
-------------------

Identifiers
~~~~~~~~~~~

One identifier can denote a variable or a constant.

A identifier is the most simple case of a left-value, this is an entity
that can be used in the left of an attribution.

Parentheses
~~~~~~~~~~~

Expressions between parentheses, “(” and “)”, has the same value as
without them. This property allows for nested parentheses.

One expression between parentheses can not be a left-value.

Indexation
~~~~~~~~~~

Indexation expressions return the same data type. They can not be used
as a left-value.

Can be used to access only one element or a range of elements. To access
one element, is required a identifier followed by an expression between
brackets. (e.g, x[10]).

To obtain a range, is required an identifier and two expressions
separated by a ’:’ and between brackets. Example:

+----------+---------------------------+------------------------------------------------------+
| x[:]     | :math:`\u27f9`            | returns from position 0 until length of variable x   |
+----------+---------------------------+------------------------------------------------------+
| x[10:]   | :math:`\u27f9`            | returns from position 10 util length of variable x   |
+----------+---------------------------+------------------------------------------------------+
| x[:10]   | :math:`\u27f9`            | returns from position 0 until 10                     |
+----------+---------------------------+------------------------------------------------------+

Can only be applied to ShortReads.

Invocation
~~~~~~~~~~

A function can only be invoked (§ [sec:invoke]) through the use of a
identifier that is specified at § [sec:functions].

Read
~~~~

The operation to look up for a given variable value can be performed by
simply using it’s name.

Unary expressions
-----------------

The operator **(-)** returns the symmetric of it’s Integer argument.

The operator **len** returns the length of a ShortRead.

Multiplicative expressions
--------------------------

This operations are only applicable to Integer values, returning the
value of the corresponding algebraic operation.

Additive expressions
--------------------

This operations are only applicable to Integer values, as the previous.

Comparative expressions
-----------------------

This operations are only applicable to Integer values and returns a
Bool. Therefore the return can either be true or false.

Equality expressions
--------------------

This operations are only applicable to Integer values, as the previous.

Attribution expressions
-----------------------

The value of the expression in the right side of the operator ’=’ is
saved in the variable, indicated by the left-value, at the left side of
the attribution operator.

To the same left-value can not be assigned right values with different
types.

Function Definition
===================

Here is provided the definition of all the available functions. To
denote a List of a given data type is used **[ X ]** where **X** is the
data type. (i.e [Symbol])

Fastq
-----

Function to load, one or more, fastQ files. An example:

::

    in = fastq('input.fq')

Argument:
~~~~~~~~~

String

Return:
~~~~~~~

ReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

none

The String argument must be a valid file path and exist. An error is
reported otherwise.

The only compression method supported for the data sets is **gzip**
(.gz).

The encoding prediction is based on the lowest quality character of
the fastQ file.

When loading a data set, quality control is performed and statistics
can be visualised in a graphical user interface (GUI).

The simple statistics calculated are percentage of guanine and cytosine
(%GC), encoding prediction, number of sequences and minimum/maximum
sequence length. The more complex statistics calculated are the mean,
median, lower quartile and upper quartile for each position of the base
pairs.

Unique
------

Function that receives a set of reads and returns a equal or smaller set
of reads. Only retains a given number of copies for each read (if there
are any duplicates). An example:

::

    input = unique(input, max_copies=3)

Argument:
~~~~~~~~~

ReadSet

Return:
~~~~~~~

ReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+---------------+-----------+------------+
| Name          | Type      | Required   |
+===============+===========+============+
| max\_copies   | Integer   |            |
+---------------+-----------+------------+

The optional argument **max\ :sub:`c`\ opies** allows to define the
number of tolerated copies (default: 1).

Is considered a copy: ShortReads with the same sequence regardless of
quality and identifier.

It’s a pure function § [sec:purefunctions].

Preprocess
----------

This function executes the given block for each read in the ReadSet.
Unless the read is **discarded**, it is transferred (after
transformations) to the output. The output is assigned to the same name
as the inputs. An example:

::

    preprocess(inputs) using |read|:
        read = read[3:]

Argument:
~~~~~~~~~

ReadSet

Return:
~~~~~~~

Void

Arguments by value:
~~~~~~~~~~~~~~~~~~~

none

This function also performs quality control on its output.

Map
---

The function map allows for a ReadSet to be mapped against a reference.
An example:

::

    mapped = map(input,reference='sacCer3')

Argument:
~~~~~~~~~

ReadSet

Return:
~~~~~~~

MappedReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------+----------+------------+
| Name        | Type     | Required   |
+=============+==========+============+
| reference   | String   |     ✓      |
+-------------+----------+------------+

The argument **reference** must be either a path to a data set or the
**name** of a provided data set by NGLess. The provided data sets of
NGLess are:

+-----------+-----------------------------+-------------+
| Name      | Description                 | Assembly    |
+===========+=============================+=============+
| sacCer3   | saccharomyces\_cerevisiae   | R64-1-1     |
+-----------+-----------------------------+-------------+
| ce10      | caenorhabditis\_elegans     | WBcel235    |
+-----------+-----------------------------+-------------+
| dm3       | drosophila\_melanogaster    | BDGP5       |
+-----------+-----------------------------+-------------+
| -         | gallus\_gallus              | Galgal4     |
+-----------+-----------------------------+-------------+
| canFam2   | canis\_familiaris           | CanFam3.1   |
+-----------+-----------------------------+-------------+
| rn4       | rattus\_norvegicus          | Rnor\_5.0   |
+-----------+-----------------------------+-------------+
| bosTau4   | bos\_taurus                 | UMD3.1      |
+-----------+-----------------------------+-------------+
| mm10      | mus\_musculus               | GRCm38      |
+-----------+-----------------------------+-------------+
| hg19      | homo\_sapiens               | GRCh37      |
+-----------+-----------------------------+-------------+

If one of the previous data sets are chosen, the data sets will be
downloaded (if they are not already locally). This data set contains the
BWA index files and a gene annotation file.

It’s a pure function § [sec:purefunctions].

Annotate
--------

Given a file with aligned sequencing reads (ReadSet) and a list of
genomic features (gff file), the function allows to annotate reads to
each feature. An example:

::

    annotated = annotate(mapped, strand={yes}, mode={union}, ambiguity={deny})

Argument:
~~~~~~~~~

MappedReadSet

Return:
~~~~~~~

AnnotatedSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------+--------------+------------+
| Name        | Type         | Required   |
+=============+==============+============+
| gff         | String       |      ✓     |
+-------------+--------------+------------+
| features    | [ Symbol ]   |            |
+-------------+--------------+------------+
| mode        | Symbol       |            |
+-------------+--------------+------------+
| ambiguity   | Symbol       |            |
+-------------+--------------+------------+
| strand      | Symbol       |            |
+-------------+--------------+------------+

The **gff** argument is required, unless it was used a data set
provided by NGLess on the map (map section). It must be a valid path to
a annotation file.

The argument **features** represents which features to keep,
discarding everything else. If nothing is provided everything is
considered to be important. The possible symbols are **{gene}**,
**{exon}** and **{cds}**.

**Mode** is a string that represents the mode to handle reads
overlapping more than one feature. The possible values for **mode** are
**{union}**, **{intersection-strict}** and **{intersection-nonempty}**
(default: {union}). For each read position is obtained features that
intersect it, which are called sets. The different modes are:

-  **union** the union of all the sets.

-  **intersection-strict** the intersection of all the sets.

-  **intersection-nonempty** the intersection of all non-empty sets.

The **ambiguity** argument allows to decide whether to count reads
that overlap with more than one feature. The possible values are {allow}
and {deny} (default: {allow}).

Argument **strand** represents whether the data is from a
strand-specific and the possible values can be **{yes}** or **{no}**
(default: {no}). For {no}, a read is always overlapping with a feature
independently of whether maps to the same or the opposite strand. For
{yes}, the read has to be mapped to the same strand as the feature.

Count
-----

Function that allows to filter the counts of features. Example:

::

    counts = count(annotated, min=2)

Argument:
~~~~~~~~~

AnnotatedSet

Return:
~~~~~~~

AnnotatedSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+----------+--------------+------------+
| Name     | Type         | Required   |
+==========+==============+============+
| counts   | [ Symbol ]   |            |
+----------+--------------+------------+
| min      | Integer      |            |
+----------+--------------+------------+

The argument **counts** represents which features to keep, discarding
everything else. The possible symbols are gene, exon and cds. If nothing
is provided everything is considered to be important.

**Min** defines the minimum amount of overlaps a given feature must
have, at least, to be kept (default: 0).

It’s a pure function § [sec:purefunctions].

Substrim
--------

Given a read, returns another that is the biggest sub-sequence with a
given minimum quality. Example:

::

    read = substrim(read, min_quality=5)

Argument:
~~~~~~~~~

ShortRead

Return:
~~~~~~~

ShortRead

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------------------+-----------+------------+
| Name                    | Type      | Required   |
+=========================+===========+============+
| min_quality             | Integer   |            |
+-------------------------+-----------+------------+

**Min\ :sub:`q`\ uality** parameter defines the minimum quality
accepted for the sub-sequence (default: 0).

It’s a pure function § [sec:purefunctions].

Write
-----

Write function allows to write a NGLessObject to Disk. Different Types
of NGLessObject are manipulated in different manners.

ReadSet
~~~~~~~

Argument:
~~~~~~~~~

ReadSet

Return:
~~~~~~~

Void

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+---------+----------+------------+
| Name    | Type     | Required   |
+=========+==========+============+
| ofile   | String   |     ✓      |
+---------+----------+------------+

The argument **ofile** is a file path to where the content is written.

MappedReadSet
~~~~~~~~~~~~~

Argument:
~~~~~~~~~

MappedReadSet

Return:
~~~~~~~

Void

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+----------+----------+------------+
| Name     | Type     | Required   |
+==========+==========+============+
| ofile    | String   |     ✓      |
+----------+----------+------------+
| format   | String   |            |
+----------+----------+------------+

**Format** can have value **{bam}** or **{sam}** (default: {sam}).

AnnotatedSet
~~~~~~~~~~~~

Argument:
~~~~~~~~~

AnnotatedSet

Return:
~~~~~~~

Void

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-----------+----------+------------+
| Name      | Type     | Required   |
+===========+==========+============+
| ofile     | String   |     ✓      |
+-----------+----------+------------+
| format    | String   |            |
+-----------+----------+------------+
| verbose   | Symbol   |            |
+-----------+----------+------------+

**Format** can have value **{csv}** or **{tsv}** (default: {tsv}).

**Verbose** allows to choose between writing a simplified or a verbose
version of the results. Possible values are **{yes}** or **{no}**
(default: {no}).

If a list of **any** of the previous mentioned data types is provided,
the **ofile** argument must use an **{index}** in the template name to
differentiate between the files in the list. For example for a list with
two elements:

::

    ofile = "../samples/CountsResult{index}.txt"

| would result in,

**“../samples/CountsResult1.txt”, “../samples/CountsResult2.txt”**

Print
-----

Print function allows to print a NGLessObject to IO.

Argument:
~~~~~~~~~

NGLessObject

Return:
~~~~~~~

Void

Arguments by value:
~~~~~~~~~~~~~~~~~~~

none

Omissions and Errors
====================

Omissions and errors will be fixed in future versions of NGLess
specification.
