# NGLess Language

This document describes the NGLess language.

## Tokenization

Tokenization follows the standard C-family rules. A word is anything that
matches `[A-Za-z_][A-Za-z_0-9]*`. The language is case-sensitive. All files are
assumed to be in UTF-8.

Both LF and CRLF are accepted as line endings (Unix-style LF is preferred).

A semicolon (;) can be used as an alternative to a new line. Any spaces (and
only space characters) following a semicolon are ignored. *This feature is
intended for inline scripts at the command line (passed with the ``-e``
option), its use for scripts is heavily discouraged and may trigger an error in
the future.*

Script-style (# to EOL), C-style (/* to \*/) and C++-style (// to EOL) comments
are all recognised. Comments are effectively removed prior to any further
parsing as are empty lines.

Strings are denoted with single or double quotes and standard backslashed
escapes apply (\\n for newline, ...).

A symbol is denoted as a token surrounded by curly braces (e.g., ``{symbol}``
or ``{gene}``).

Integers are specified as decimals ``[0-9]+`` or as hexadecimals
``0x[0-9a-fA-F]+``.

## Version declaration

The first line (ignoring comments and empty lines) of an NGLess file MUST be a
version declaration:

    ngless "0.9"


## Module Import Statements

Following the version statement, optional import statements are allowed, using
the syntax `import "<MODULE>" version "<VERSION>"`. For example:

    import "batch" version "1.0"

This statement indicates that the ``batch`` module, version ``1.0`` should be
used in this script. Module versions are independent of NGLess versions.

Only a predefined set of modules can be imported (these are shipped with
NGLess). To import user-written modules, the user MUST use the _local import_
statement, e.g.:

    local import "batch" version "1.0"

Import statements MUST immediately follow the version declaration

### Blocks

Blocks are defined by indentation in multiples of 4 spaces. To avoid confusion,
TAB characters are not allowed.

Blocks are used for conditionals and `using` statements.

## Data types

NGless supports the following basic types:

- String
- Integer
- Double
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

### String

A string can start with either a quote `(U+0022, ")` or a single quote
`(U+0027,')` or and end with the same character. They can contain any number
of characters.

Special sequences start with `\`. Standard backslashed escapes can be
used as `LF` and `CR` (`\n` and `\r` respectively), quotation marks
(`\'`) or slash (`\\`).

### Integer

Integers are specified as decimals `[0-9]+` or as hexadecimals
`0x[0-9a-fA-F]+`. The prefix `-` denotes a negative number.

### Double

Doubles are specified as decimals `[0-9]+` with the decimal point serving as a
separator. The prefix `-` denotes a negative number.

Doubles and Integers are considered numeric types.

### Boolean

The two boolean constants are `True` and `False` (which can also be written
`true` or `false`).

### Symbol

A symbol is denoted as a token surrounded by curly braces (e.g., `{symbol}` or
`{drop}`). Symbols are used as function arguments to indicate that there is
only a limited set of allowed values for that argument. Additionally, unlike
Strings, no operations can be performed with Symbols.

## Variables

NGless is a statically typed language and variables are typed. Types are
automatically inferred from context.

Assignment is performed with `=` operator:

    variable = value

A variable that is all uppercase is a constant and can only be assigned to
once.


## Operators

### Unary

The operator `(-)` returns the symmetric of its numeric argument.

The operator `len` returns the length of a ShortRead.

The operator `not` negates its boolean argument

## Binary

All operators can only be applied to numeric types. Mixing integers and doubles
returns a double. The following binary operators are used for arithmetic:

    + - < > >= <= == !=

The `+` operator can also perform concatenation of String objects.


The `</>` operator is used to concatenate two Strings while also adding a '/'
character between them. This is useful for concatenating file paths.

## Indexing

Can be used to access only one element or a range of elements in a ShortRead.
To access one element, is required an identifier followed by an expression
between brackets. (e.g, x[10]).

To obtain a range, is required an identifier and two expressions separated by a
':' and between brackets. Example:

* `x[:]` - from position 0 until length of variable x
* `x[10:]` - from position 10 until length of variable x
* `x[:10]` - from position 0 until 10

## Conditionals

Conditionals work as in Python. For example:

    if 5 > 10:
       val = 10
    else:
       val = 20

## Functions

Functions are called with parentheses:

    result = f(arg, arg1=2)

Functions have a single positional parameter, all other must be given by name:

    unique(reads, max_copies=2)

The exception is constructs which take a block: they take a single positional
parameter and a block. The block is passed using the ``using`` keyword:

    reads = preprocess(reads) using |read|:
        block
        ...

The `|read|` syntax defines an unnamed (lambda) function, which takes a
variable called `read`. The function body is the following block.

There is no possibility of defining new functions within the language. Only
built-in functions or those added by modules can be used.

## Methods

Methods are called using the syntax `object . methodName ( <ARGS> )`. As with
functions, one argument may be unnamed, all others must be passed by name.

## Grammar


This is the extended Backus-Naur form grammar for the NGLess language (using
the [ISO
14977](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
conventions). Briefly,  the comma (`,`) is used for concatenation, `[x]`
denotes _optional_, and `{x}` denotes _zero or more of `x`_.


    string = ? a quoted string, produced by the tokenizer ? ;
    word = ? a word produced by the tokenizer ? ;
    
    eol =
        ';'
        | '\n' {'\n'}
        ;
    
    
    ngless = [header], body;
    
    header = {eol}, ngless_version, {eol}, {import}, {eol}
    
    ngless_version = "ngless", string, eol ;
    
    import = ["local"],  "import", string, "version", string, eol ;
    
    body = {expression, eol} ;
    
    expression =
                conditional
                | "discard"
                | "continue"
                | assignment
                | innerexpression
                ;
    
    innerexpression = left_expression, binop, innerexpression
                        | left_expression
                        ;
    
    left_expression =  uoperator
                        | method_call
                        | indexexpr
                        | base_expression
                        ;
    
    base_expression = pexpression
                       | funccall
                       | listexpr
                       | constant
                       | variable
                       ;
    
    pexpression = '(', innerexpression, ')' ;
    
    constant =
            "true"
            | "True"
            | "false"
            | "False"
            | double
            | integer
            | symbol
            ;
    
    double = integer, '.', integer ;
    integer = digit, {digit} ;
    digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
    symbol = '{', word, '}' ;
    
    
    indentation = ' ', {' '} ;
    binop = '+' | '-' | '*' | "!=" | "==" | "</>" | "<=" | "<" | ">=" | ">" | "+" | "-" ;
    
    uoperator =
            lenop
            | unary_minus
            | not_expr
            ;
    
    lenop = "len", '(', expression, ')'
    unary_minus = '-', base_expression ;
    not_expr = "not", innerexpression ;
    
    funccall = paired
            | word,  '(', innerexpression, kwargs, ')', [ funcblock ]
            ;
    
    (* paired is a special-case function with two arguments *)
    paired = "paired", '(', innerexpression, ',', innerexpression,  kwargs ;

    funcblock = "using", '|', [ variablelist ], '|', ':', block ;
    
    
    kwargs = {',', variable, '=', innerexpression} ;
    
    assignment = variable, '=', expression ;
    
    method_call = base_expression, '.', word, '(', [ method_args ], ')';
    method_args =
            innerexpression, kwargs
            | variable, '=', innerexpression, kwargs
            ; (* note that kwargs is defined as starting with a comma *)
    
    indexexpr = base_expression, '[', [ indexing ], ']' ;
    
    indexing = [ innerexpression  ], ':', [ innerexpression ] ;
    
    listexpr = '[', [ list_contents ] , ']' ;
    list_contents = innerexpression, {',', innerexpression } ;
    
    conditional = "if",  innerexpression, ':',  block, [ elseblock ] ;
    elseblock = "else", ':', block ;
    block = eol, indentation, expression, eol, {indentation, expression, eol} ;
    
    variablelist = variable, {',', variable} ;
    variable = word ;

