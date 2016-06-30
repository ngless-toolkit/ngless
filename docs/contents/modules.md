# Modules

To add a module to ngless there are two options: *external* or *internal*
modules. External modules are the simplest option.

## External modules

External scripts can perform two tasks:

1. Add new references to ngless
2. Add functions to ngless

### New references

A module can add references to ngless which can then be used in the `map()`
call using the `reference` argument.

Like everything else in ngless, these are versioned for reproducibility so that
the resulting script implicitly encodes the exact version of the databases used.

### New functions

An external module is a way to have functions in ngless map to command line
calls to your scripts.

## How to define an external module

You can use the [example
module](https://github.com/luispedro/ngless/blob/master/Modules/example-cmd.ngm/0.0/module.yaml)
in the ngless source for inspiration. That is a complete, functional module.

A module is defined by an ``YaML`` file.

Every module has a name and a version:

    name: 'module'
    version: '0.0.0'

Everything else is optional.

An ``init`` section defines an initialization command. This will be run
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
``NGLESS_MODULE_DIR`` to the module directory.

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
            argName: "./run-test.sh"

will enable the user to call a function ``test()`` which will translate into a
call to the ``run-test.sh`` script (see the note above about paths).

You can also add arguments to your function, naturally. Remember that ngless
functions can have only one unnamed argument and any number of named arguments.
To specify the unnamed argument add a ``arg1`` section:

            arg1:
                filetype: <one of "tsv"/"fq1"/"fq2"/"fq3"/"sam"/"bam"/"sam_or_bam"/"tsv">
                can_gzip: true/false

Finally, additional argument are specified by a list called ``additional``:

            additional:
                -
                    name: <name>
                    atype: <one of 'str'/'flag'/'int'/'option'>
                    def: <default value>
                    required: true/false

Arguments of type ``flag`` will be passed to your script using the form
``--name`` if their value is true.

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
trigger a warning about this anomalous situation).

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

### Citation

Finally, if you wish to, you can add a citation:

    citation: "A paper which you want to be listed when users import your module"

This will be printed out whenever users use your module and thus will help you
get exposure.

## Internal Modules

This is very advanced as it requires writing Haskell code which can then
interact very deeply with the rest of ngless.

