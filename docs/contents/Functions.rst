.. _Functions:

=========
Functions
=========

Fastq
-----

Function to load, one or more, FastQ files, for example::

  in = fastq('input.fq')

Argument:
~~~~~~~~~
String

Return:
~~~~~~~
ReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~
+---------------+----------------------+------------+----------------+
| Name          | Type                 | Required   | Default Value  |
+===============+======================+============+================+
| encoding      | Symbol               |  no        | {auto}         |
+               + ({auto}, {33}, {64}, +            +                +
+               + {sanger}, {solexa})  +            +                +
+               +                      +            +                +
+---------------+----------------------+------------+----------------+

Possible values for ``encoding`` are:

- ``{sanger}`` or ``{33}`` assumes that the file is encoded using sanger
  format. This is appropriate for newer Illumina outputs.
- ``{solexa}`` or ``{64}`` assumes that the file is encoded with a 64 offset.
  This is used for older Illumina/Solexa machines.
- ``{auto}``: use auto detection

When loading a data set, quality control is carried out and statistics can be
visualised in a graphical user interface (GUI). Statistics calculated are:

- percentage of guanine and cytosine (%GC)
- number of sequences
- minimum/maximum sequence length
- mean, median, lower quartile and upper quality quartile for each sequence
  position

If not specified, the encoding is guessed from the file.

Gzip and bzip2 compressed files are transparently supported (determined by file
extension, ``.gz`` and ``.bz2`` for gzip and bzip2 respectively).

samfile
-------

Loads a SAM file::

    s = samfile('input.sam')

This function takes no keyword arguments. BAM files are also supported (determined by the filename), as are ``sam.gz`` files.

Returns
~~~~~~~

MappedReadSet

Unique

countfile
---------

Loads a TSV file::

    c = countfile('table.tsv')

This function takes no keyword arguments. If the filename ends with ".gz", it is assumed to be a gzipped file.

Returns
~~~~~~~

CountTable

Unique
------

Function that given a set of reads, returns another which only retains a
set number of copies of each read (if there are any duplicates). An
example::

    input = unique(input, max_copies=3)

Argument:
~~~~~~~~~

ReadSet

Return:
~~~~~~~

ReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+---------------+--------------+------------+----------------+
| Name          | Type         | Required   | Default Value  |
+===============+==============+============+================+
| max\_copies   | Integer      |  no        | 2              |
+---------------+--------------+------------+----------------+

The optional argument **max_copies** allows to define the number of tolerated
copies (default: 2).

Two short reads with the same nucleotide sequence are considered copies,
independently of quality and identifiers.

Preprocess
----------

This function executes the given block for each read in the ReadSet.  Unless
the read is **discarded**, it is transferred (after transformations) to the
output. The output is assigned to the same name as the inputs. For example::

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

+---------------+--------------+------------+----------------+
| Name          | Type         | Required   | Default Value  |
+===============+==============+============+================+
| keep\_singles | bool         |  no        | true           |
+---------------+--------------+------------+----------------+

When a paired-end input is being preprocessed in single-mode (i.e., each mate
is preprocessed independently, it can happen that on eof the mates is
discarded, while the other is kept). The default is to collect these into the
singles pile. If ``keep_singles`` if false, however, they are discarded.

This function also performs quality control on its output.

Map
---

The function map, maps a ReadSet to reference. For example::

    mapped = map(input, reference='sacCer3')
    mapped = map(input, fafile='ref.fa')

Argument:
~~~~~~~~~

ReadSet

Return:
~~~~~~~

MappedReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------+-------------+------------+----------------+
| Name        | Type        | Required   | Default Value  |
+=============+=============+============+================+
| reference   | String      | no         | -              |
+-------------+-------------+------------+----------------+
| fafile      | String      | no         | -              |
+-------------+-------------+------------+----------------+

The user must provide either a path to a FASTA file in the ``fafile`` argument
or the name of a builtin reference using the ``reference`` argument.

NGLess provides the following builtin datasets:

+-----------+-----------------------------+-------------+
| Name      | Description                 | Assembly    |
+===========+=============================+=============+
| sacCer3   | saccharomyces\_cerevisiae   | R64-1-1     |
+-----------+-----------------------------+-------------+
| ce10      | caenorhabditis\_elegans     | WBcel235    |
+-----------+-----------------------------+-------------+
| dm3       | drosophila\_melanogaster    | BDGP5       |
+-----------+-----------------------------+-------------+
| gg4       | gallus\_gallus              | Galgal4     |
+-----------+-----------------------------+-------------+
| canFam2   | canis\_familiaris           | CanFam3.1   |
+-----------+-----------------------------+-------------+
| rn4       | rattus\_norvegicus          | Rnor\_5.0   |
+-----------+-----------------------------+-------------+
| bosTau4   | bos\_taurus                 | UMD3.1      |
+-----------+-----------------------------+-------------+
| mm10      | mus\_musculus               | GRCm38      |
+-----------+-----------------------------+-------------+
| hg19      | homo\_sapiens               | GRCh38      |
+-----------+-----------------------------+-------------+

To use any of these, pass in the name as the reference value::

    mapped_hg19 = map(input, reference='hg19')

Ngless does not ship with any of these datasets, but they are downloaded
lazily: i.e., the first time you use them, ngless will download and cache them.

select
------

`select` filters a MappedReadSet. For example::

    mapped = select(mapped, keep_if=[{mapped}])

Argument:
~~~~~~~~~

MappedReadSet

Return:
~~~~~~~

MappedReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------+-------------+------------+----------------+
| Name        | Type        | Required   | Default Value  |
+=============+=============+============+================+
| keep_if     | [Symbol]    | no         | -              |
+-------------+-------------+------------+----------------+
| drop_if     | [Symbol]    | no         | -              |
+-------------+-------------+------------+----------------+

At least one of ``keep_if`` or ``drop_if`` should be passed, but not both. They accept the following symbols:

- ``{mapped}``: the read mapped
- ``{unmapped}``: the read did not map
- ``{unique}``: the read mapped to a unique location

If ``keep_if`` is used, then reads are kept if they pass **all the conditions**.
If ``drop_if`` they are discarded if they fail to **any condition**.




Count
-----

Given a file with aligned sequencing reads (ReadSet), ``count()`` will produce
a counts table depending on the arguments passed. For example::

    counts = count(mapped, min=2, mode={union}, keep_ambiguous=True, multiple={dist1})

Argument:
~~~~~~~~~

MappedReadSet

Return:
~~~~~~~

CountTable

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------------+-----------------+------------+----------------+
| Name              | Type            | Required   | Default value  |
+===================+=================+============+================+
| gff\_file         | String          | no*        |  -             |
+-------------------+-----------------+------------+----------------+
| functional\_map   | String          | no*        |  -             |
+-------------------+-----------------+------------+----------------+
| features          | [ String ]      | no         | 'gene'         |
+-------------------+-----------------+------------+----------------+
| mode              | Symbol          | no         | {union}        |
+-------------------+-----------------+------------+----------------+
| keep\_ambiguous   | Bool            | no         | true           |
+-------------------+-----------------+------------+----------------+
| multiple          | Symbol          | no         | {dist1}        |
+-------------------+-----------------+------------+----------------+
| strand            | Bool            | no         | false          |
+-------------------+-----------------+------------+----------------+
| min               | Integer         | no         | 0              |
+-------------------+-----------------+------------+----------------+
| discard_zero      | Bool            | no         | false          |
+-------------------+-----------------+------------+----------------+


If the features to count are ``['seqname']``, then each read will be assigned
to the name of reference it matched and only an input set of mapped reads is
necessary. For other features, you will need extra information. This can be
passed using the ``gff_file`` or ``functional_map`` arguments. If you had
previously used a ``reference`` argument for the ``map()`` function, then
you can also leave this argument empty and ngless will do the right thing.

``features``: which features to count.

``mode`` indicates how to handle reads that partially overlap a features.
Possible values for ``mode`` are ``{union}``, ``{intersection-strict}``, and
``{intersection-nonempty}`` (default: ``{union}``). For each read position are
obtained features that intersect it, which is known as sets. The different
modes are:

-  ``{union}`` the union of all the sets.
-  ``{intersection-strict}`` the intersection of all the sets.
-  ``{intersection-nonempty}`` the intersection of all non-empty sets.

The ``keep_ambiguous`` argument is an opportunity to decide whether to count
reads that overlap with more than one feature or that were multiply mapped to
several genomic locations, which themselves correspond to more than one
feature.

Argument ``strand`` represents whether the data are from a strand-specific
(default is ``false``). When the data is not strand-specific, a read is always
overlapping with a feature independently of whether maps to the same or the
opposite strand. For strand-specific data, the read has to be mapped to the
same strand as the feature.

``min`` defines the minimum amount of overlaps a given feature must have, at
least, to be kept (default: 0, i.e., keep all counts). If you just want to
discard features that are exactly zero, you should set the ``discard_zero``
argument to True.

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

+-------------------------+--------------+------------+----------------+
| Name                    | Type         | Required   | Default Value  |
+=========================+==============+============+================+
| min_quality             | Integer      |  no        |	0              |
+-------------------------+--------------+------------+----------------+

**Min_quality** parameter defines the minimum quality
accepted for the sub-sequence (default: 0).

Write
-----

Writes an object to disk.


ReadSet
~~~~~~~

Argument:
#########

ReadSet

Return:
#######

Void

Arguments by value:
###################

+---------+-------------+------------+----------------+
| Name    | Type        | Required   | Default Value  |
+=========+=============+============+================+
| ofile   | String      | yes        | -              |
+---------+-------------+------------+----------------+

The argument **ofile** is a file path to where the content is written.

MappedReadSet
~~~~~~~~~~~~~~~~~

Argument:
##########

MappedReadSet

Return:
##########

Void

Arguments by value:
###################

+----------+-------------+------------+----------------+
| Name     | Type        | Required   | Default Value  |
+==========+=============+============+================+
| ofile    | String      |  yes       | -              |
+----------+-------------+------------+----------------+
| format   | String      |  no        | {sam}          |
+----------+-------------+------------+----------------+

**Format** can have value **{bam}** or **{sam}** (default: {sam}).

Arguments by value:
###################

+----------+-------------+------------+----------------+
| Name     | Type        | Required   | Default Value  |
+==========+=============+============+================+
| ofile    | String      |  yes       | -              |
+----------+-------------+------------+----------------+
| format   | String      |  no        | {tsv}          |
+----------+-------------+------------+----------------+
| verbose  | Bool        |  no        | false          |
+----------+-------------+------------+----------------+

**Format** can have value ``{csv}`` or ``{tsv}`` (default: ``{tsv}``).

If a list of **any** of the previously mentioned data types is provided, the
``ofile`` argument must use an **{index}** in the template name to
differentiate between the files in the list. For example for a list with two
elements::

    ofile = "result{index}.txt"

| would result in ``result1.txt``, ``result2.txt``,...

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
