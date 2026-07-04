Mapping
=======

Mapping is one of the major functions of NGLess. Here we describe, in more
detail, some of its functionality.

Mapping is implemented using `bwa <https://bio-bwa.sourceforge.net/>`__ by
default. NGLess resolves the ``bwa`` executable from ``NGLESS_BWA_BIN`` or from
``PATH`` and records the tool version in the generated index filenames.

By default, bwa is called with default parameters. If the ``mode_all`` argument
is set to true, then ``-a`` is passed to ``bwa``.

Low memory mode
---------------

As databases get very large, memory requirements can grow very large. In order
to make large databases accessible to users without access to large memory
machines, NGLess implements a simple heuristic: it splits the input database
into smaller blocks, processes each one in turn and combines the results at the
end.

To enable low-memory mode, use the ``block_size_megabases`` in the script. Set
it to a value that is less than the available memory. Note that this *does
change* the results (although the impact is limited).

A FAQ is why the memory requirements are not a configuration option and must be
specified in the script. As low memory mode is heuristic, it can potentially
*change* results. As NGLess aims to capture all parameters that can change the
result **inside** the script, it must be specified as an argument to ``map()``.

Using minimap2
--------------

You can use minimap2 as an alternative mapper by importing the minimap2 module
and passing ``mapper="minimap2"``::

    ngless "1.6"
    import "minimap2" version "1.6"

    input = ....

    mapped = map(input, fafile="reference.fa", mapper="minimap2")

