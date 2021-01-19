# Modules

To add a module to ngless there are two options: *external* or *internal*
modules. External modules are the simplest option.

## External modules

External modules can perform two tasks:

1. Add new references to ngless
2. Add functions to ngless

Adding references makes them available to the `map()` call using the
`reference` argument and (optionally) allows for calls to `count()` without
specifying any annotation file.

Like everything else in ngless, these are versioned for reproducibility so that
the resulting script implicitly encodes the exact version of the databases used.

Functions in external modules map to command line calls to a script you
provide.

## How to define an external module

You can use the [example
module](https://github.com/ngless-toolkit/ngless/blob/master/Modules/example-cmd.ngm/0.0/module.yaml)
in the ngless source for inspiration. That is a complete, functional module.

A module is defined by an ``YaML`` file.

Every module has a name and a version:

    name: 'module'
    version: '0.0.0'

Everything else is optional.

### References

References are added with a *references* section, which is a list of
references. A reference contains a ``fasta-file`` and (optionally) a
``gtf-file``. For example:

    references:
        -
            name: 'ref'
            fasta-file: 'data/reference.fna'
            gtf-file: 'data/reference.gtf.gz'

Note that the paths are relative to the module directory. The GTF file may be
gzipped.

### Initialization

An `init` section defines an initialization command. This will be run
**before** anything else in any script which imports this module. The intention
is that the module can check for any dependencies and provide the user with an
early error message instead of failing later after. For example:

    init:
        init_cmd: './init.sh'
        init_args:
            - "Hello"
            - "World"

will cause ngless to run the command ``./init.sh Hello World`` whenever a user
imports the module.

**A note about paths**: paths you define in the module.yaml file are *relative
to the Yaml file itself*. Thus you put all the necessary scripts and data in
the module directory. However, the scripts are run with the current working
directory of wherever the user is running the ngless protocol (so that any
relative paths that the user specifies work as expected). To find your data
files inside your module, ngless sets the environmental variable
``NGLESS_MODULE_DIR`` as the path to the module directory.


### Functions

To add new functions, use a `functions` section, which should contain a list of
functions encoded in YaML format. Each function has a few required arguments:

``nglName``
    the name by which the function will be called **inside** of an ngless
    script.

``arg0``
    the script to call for this function. Note that the user will never see
    this.


For example:

    functions:
        -
            nglName: "test"
            arg0: "./run-test.sh"

will enable the user to call a function ``test()`` which will translate into a
call to the ``run-test.sh`` script (see the note above about paths).

You can also add arguments to your function, naturally. Remember that ngless
functions can have only one unnamed argument and any number of named arguments.
To specify the unnamed argument add a ``arg1`` section, with the key ``atype``
(argument type):

            arg1:
                atype: <one of 'readset'/'mappedreadset'/'counts'/'str'/'flag'/'int'/'option'>

The arguments of type *readset*, *mappedreadset*, and *counts* are passed as
paths to a file on disk. **Your command is assumed to not change these, but
make a copy if necessary. Bad things will happen if you change the files.**
You can specify more details on which kind of file you expect with the
following optional arguments:

                filetype: <one of "tsv"/"fq1"/"fq2"/"fq3"/"sam"/"bam"/"sam_or_bam"/"tsv">
                can_gzip: true/false
                can_bzip2: true/false
                can_stream: true/false

The flags ``can_gzip``/``can_bzip2`` indicate whether your script can accept
compressed files (default: *False*). ``can_stream`` indicates whether the input
can be a pipe (default: *False*, which means that an intermediate file will
always be used).

For example, if your tool wants a SAM file (and never a BAM file), you can write:

            arg1:
                atype: mappedreadset
                filetype: sam

``ngless`` will ensure that your tool does receive a SAM file (including
converting BAM to SAM if necessary).

Finally, additional argument are specified by a list called ``additional``.
Entries in this list have exactly the same format as the ``arg1`` entry, except
that they have a few extra fields. The extra field ``name`` is mandatory, while
everything else is optional:

            additional:
                -
                    name: <name>
                    atype: <as for arg1: 'readset'/'mappedreadset'/...>
                    def: <default value>
                    required: true/false

Arguments of type ``flag`` have an optional extra argument, ``when-true`` which
is a list of strings which will be passed as extra arguments when the flag is
true. You can also just specify a single string. If ``when-true`` is missing,
ngless will pass an option of the form ``--name`` (i.e., a double-dash then the
name used). For example:

            additional:
                -
                    name: verbose
                    atype: flag
                    def: false
                    when-true: "-v"
                -
                    name: complete
                    atype: flag
                    def: false
                    when-true:
                        - "--output=complete"
                        - "--no-filter"


All other argument types are passed to your script using the syntax
``--name=value`` if they are present or if a default has been provided.

Arguments of type ``option`` map to symbols in ngless and require you to add an
additional field ``allowed`` specifying the universe of allowed symbols. Ngless
will check that the user specifies arguments from the allowable universe. For
example:

            additional:
                -
                    atype: 'option'
                    name: 'verbosity'
                    def: 'quiet'
                    allowed:
                        - 'quiet'
                        - 'normal'
                        - 'loud'

If you do not have a fixed universe for your argument, then it should be a
``str`` argument.

The ``required`` flag determines whether the argument is required. Note that
arguments with a default argument are automatically optional (ngless may
trigger a warning if you mark an argument with a default as required).

To return a value, you must request that ngless generate a new temporary file
for the script to generate output to. Therefore, you need to specify a
``return`` section, with three parameters: ``rtype`` (return type, see below),
``name`` the name of the argument to use, and ``extension`` the file extension
of the output type.

        return:
            rtype: "counts"
            name: "ofile"
            extension: "sam"

``rtype`` must be one of ``"void"``, ``"counts"`` or ``"mappedreadset"``.
Returning ``readset`` isn't currently supported.

If you plan to make use of [search path expansion](searchpath.html), in order
for NGLess to expand the argument prior to passing it to the external module
you need to set ``atype: "str"`` and ``expand_searchpath: true``.

        additional:
            -
                atype: 'str'
                name: 'reference'
                expand_searchpath: true

### Citation

Finally, if you wish to, you can add one or more citations:

    citation: "A paper which you want to be listed when users import your module"


This will be printed out whenever users use your module and thus will help you
get exposure.

If you have more than one citation, you can use the ``citations`` key and
provide a list:

    citations:
        - "Paper 1"
        - "Paper 2"

## Minimal NGLess Version

External modules can specify a minimal NGLess version that they need to run.
This is optional, but if it is used, you need to additionally supply a reason
for the requirement (using the aptly-named `reason` field):

    min-ngless-version:
        min-version: "1.3"
        reason: "The min-ngless-version field is only supported since NGLess 1.3"


## Internal Modules

This is very advanced as it requires writing Haskell code which can then
interact very deeply with the rest of ngless.

For an example, you can look at the [example internal
module](https://github.com/ngless-toolkit/ngless/blob/master/NGLess/StandardModules/Example.hs).
If you want to get started, you can ask about details on the [ngless user
mailing list](https://groups.google.com/forum/#!forum/ngless).


