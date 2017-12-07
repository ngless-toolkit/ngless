Mapping
=======

Mapping is one of the major functions of NGLess. Here we describe, in more
detail, some of its functionality.

Mapping is implemented using `bwa <http://bio-bwa.sourceforge.net/>`__. As of
version 0.6, NGLess uses *bwa 0.7.15*.

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
it to a value that is less than the available memory.

A FAQ is why the memory requirements are not a configuration option and must be
specified in the script. As low memory mode is heuristic, it can potentially
*change* results. As NGLess aims to capture all parameters that can change the
result **inside** the script, it must be specified as an argument to ``map()``.

