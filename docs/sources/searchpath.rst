=====================
Search path expansion
=====================

.. note::
    Search path expansion is a very powerful feature. It can be abused to
    defeat NGLess' `reproducibility mechanisms <reproducible.html>`__ and to
    obsfucate which reference information is being used. However, if used
    correctly, it can greatly simplify file management and **enhance**
    reproducibility.

NGLess supports a search path system to find references. Certain functions
(such as `map() <Functions.html#map>`__) support *search path expansion*. For
example, you can write::

    map(input, fafile="<>/my-reference.fa")

Then if the search path consists of ``"/opt/ngless-references/"``, the expanded
version will be ``"/opt/ngless-references/my-reference.fa"``.

## Named and unnamed search paths

You can have named and unnamed paths in your search path. The rules are a bit
complex (see below), but it makes sense if you see examples::

    map(input, fafile="<references>/my-reference.fa")

With the search path ``['references=/opt/ngless-refs']`` will result in
``'/opt/ngless-refs/my-reference.fa'``.

With the search path ``['internal=/opt/ngless-internal',
'references=/opt/ngless-refs']`` will also result in
``'/opt/ngless-refs/my-reference.fa'`` as the *internal* path will not be
matched.

With the search path ``['internal=/opt/ngless-internal',
'references=/opt/ngless-refs', '/opt/ngless-all']`` now it will result in
``['/opt/ngless-refs/my-reference.fa', '/opt/ngless-all/my-reference.fa']`` as
the unnamed path will always match. Since there is more than one result, both
are checked (in order).

Using ``<>`` (as in the example above) will use only unnamed paths.

## Setting the search path

The search path can be passed on the command line::

    ngless script.ngl --search-path "references=/opt/ngless"

Alternatively, you can set it on the ngless configuration file::

    search-path = ["references=/opt/ngless"]

Note that **the search path is a list**, even if it contains a single element.

## Rules

1. If a path matches ``<([^>]*)>``, then it is path expanded.
2. The search path (which is a list of named and unnamed search paths) if
   filter. A path is kept on the list if it is an unnamed paht or if the name
   matches the requested pattern (``<references>`` requests "references";
   ``<>`` never matches so that only unnamed paths are kept).
3. Paths are tested in order and the first path referring to an existing file
   is kept.

Similarly


