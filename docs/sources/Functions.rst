
.. _Functions:

=========
Functions
=========

These are the built-in ngless functions. Make sure to check the `standard
library <stdlib.html>`__ as well.

fastq
-----

Function to load a FastQ file::

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
- ``{auto}``: use auto detection. This is the default.

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


paired
------

Function to load a paired-end sample, from two FastQ files::

  in = paired('input.1.fq', 'input.2.fq', singles='input.3.fq')

``paired()`` is an exceptional function which takes **two** unnamed arguments,
specifying the two read files (first mate and second mate) and an optional
``singles`` file (which contains unpaired reads).

Argument:
~~~~~~~~~
String, String

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
+---------------+----------------------+------------+----------------+
| singles       | String               | no         | -              |
+---------------+----------------------+------------+----------------+

The ``encoding`` argument has the same meaning as for the ``fastq()`` function:

- ``{sanger}`` or ``{33}`` assumes that the file is encoded using sanger
  format. This is appropriate for newer Illumina outputs.
- ``{solexa}`` or ``{64}`` assumes that the file is encoded with a 64 offset.
  This is used for older Illumina/Solexa machines.
- ``{auto}``: use auto detection. This is the default.


samfile
-------

Loads a SAM file::

    s = samfile('input.sam')

This function takes no keyword arguments. BAM files are also supported (determined by the filename), as are ``sam.gz`` files.

Returns
~~~~~~~

MappedReadSet

qcstats
-------

.. versionadded:: 0.6
    This functionality was not available prior to 0.6

Returns the auto-computed statistics::

    write(qcstats({fastq}), ofile='fqstats.txt')


Returns
~~~~~~~

CountsTable

Argument
~~~~~~~~

``{fastq}``: FastQ statistics
``{mapping}``: Mapping statistics


countfile
---------

Loads a TSV file::

    c = countfile('table.tsv')

This function takes no keyword arguments. If the filename ends with ".gz", it is assumed to be a gzipped file.

Returns
~~~~~~~

CountTable

as_reads
--------

Converts from a ``MappedReadSet`` to a ``ReadSet``::

    reads = as_reads(samfile('input.sam'))


unique
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

This function is currently limited to single-end samples.

preprocess
----------

This function executes the given block for each read in the ReadSet.  Unless
the read is **discarded**, it is transferred (after transformations) to the
output. For example::

    inputs = preprocess(inputs) using |read|:
        read = read[3:]

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
| keep\_singles | bool         |  no        | true           |
+---------------+--------------+------------+----------------+

When a paired-end input is being preprocessed in single-mode (i.e., each mate
is preprocessed independently, it can happen that on eof the mates is
discarded, while the other is kept). The default is to collect these into the
singles pile. If ``keep_singles`` if false, however, they are discarded.

This function also performs quality control on its output.

map
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

+------------------------+-------------+------------+----------------+
| Name                   | Type        | Required   | Default Value  |
+========================+=============+============+================+
| reference              | String      | no         | -              |
+------------------------+-------------+------------+----------------+
| fafile                 | String      | no         | -              |
+------------------------+-------------+------------+----------------+
| block_size_megabases   | Integer     | no         | -              |
+------------------------+-------------+------------+----------------+
| mode_all               | Bool        | no         | -              |
+------------------------+-------------+------------+----------------+

The user must provide either a path to a FASTA file in the ``fafile`` argument
or the name of a builtin reference using the ``reference`` argument. The
``fafile`` argument supports `search path expansion <searchpath.html>`__.

A list of datasets provided by NGLess can be found at :ref:`Organisms`.

To use any of these, pass in the name as the reference value::

    mapped_hg19 = map(input, reference='hg19')

Ngless does not ship with any of these datasets, but they are downloaded
lazily: i.e., the first time you use them, ngless will download and cache them.

The option ``block_size_megabases`` turns on low memory mode (see the
corresponding section in the `mapping documentation <mapping.html>`__)

The option ``mode_all=True`` can be passed to include all alignments of both
single and paired-end reads in the output SAM/BAM.

mapstats
--------

Computes some basic statistics from a set of mapped reads (number of reads,
number mapped, number uniquely mapped).

Argument
~~~~~~~~
MappedReadSet

Return
~~~~~~
CountTable

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
| paired      | Bool        | no         | true           |
+-------------+-------------+------------+----------------+

At least one of ``keep_if`` or ``drop_if`` should be passed, but not both. They accept the following symbols:

- ``{mapped}``: the read mapped
- ``{unmapped}``: the read did not map
- ``{unique}``: the read mapped to a unique location

If ``keep_if`` is used, then reads are kept if they pass **all the conditions**.
If ``drop_if`` they are discarded if they fail to **any condition**.

By default, ``select`` operates on a paired-end read as a whole. If
``paired=False`` is passed, however, then link between the two mates is not
considered and each read is processed independently.

count
-----

Given a file with aligned sequencing reads (ReadSet), ``count()`` will produce
a counts table depending on the arguments passed. For example::

    counts = count(mapped, min=2, mode={union}, multiple={dist1})

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
| subfeatures       | [ String ]      | no         | -              |
+-------------------+-----------------+------------+----------------+
| mode              | Symbol          | no         | {union}        |
+-------------------+-----------------+------------+----------------+
| multiple          | Symbol          | no         | {dist1}        |
+-------------------+-----------------+------------+----------------+
| strand            | Bool            | no         | false          |
+-------------------+-----------------+------------+----------------+
| normalization     | Symbol          | no         | {raw}          |
+-------------------+-----------------+------------+----------------+
| include_minus1    | Bool            | no         | true           |
+-------------------+-----------------+------------+----------------+
| min               | Integer         | no         | 0              |
+-------------------+-----------------+------------+----------------+
| discard_zeros     | Bool            | no         | false          |
+-------------------+-----------------+------------+----------------+


If the features to count are ``['seqname']``, then each read will be assigned
to the name of reference it matched and only an input set of mapped reads is
necessary. For other features, you will need extra information. This can be
passed using the ``gff_file`` or ``functional_map`` arguments. If you had
previously used a ``reference`` argument for the ``map()`` function, then
you can also leave this argument empty and ngless will do the right thing.

The ``gff_file`` and ``functional_map`` arguments support `search path
expansion <searchpath.html>`__.

``features``: which features to count. If a GFF file is used, this refers to
the "features" field.

``subfeatures``: this is useful in GFF-mode as the same feature can encode
multiple attributes (or, in NGLess parlance, "subfeatures"). By default, NGLess
will look for the ``"ID"`` or ``"gene_id"`` attributes.

``mode`` indicates how to handle reads that partially overlap a features.
Possible values for ``mode`` are ``{union}``, ``{intersection-strict}``, and
``{intersection-nonempty}`` (default: ``{union}``). For each read position are
obtained features that intersect it, which is known as sets. The different
modes are:

-  ``{union}`` the union of all the sets.
-  ``{intersection-strict}`` the intersection of all the sets.
-  ``{intersection-nonempty}`` the intersection of all non-empty sets.

How to handle multiple mappers (inserts which have more than one "hit" in the
reference) is defined by the ``multiple`` argument:

- ``{unique_only}``: only use uniquely mapped inserts
- ``{all1}``: count all hits separately. An insert mapping to 4 locations adds 1 to each location
- ``{1overN}``: fractionally distribute multiple mappers. An insert mapping to 4 locations adds 0.25 to each location
- ``{dist1}``: distribute multiple reads based on uniquely mapped reads. An insert mapping to 4 locations adds to these in proportion to how uniquely mapped inserts are distributed among these 4 locations.

Argument ``strand`` represents whether the data are from a strand-specific
(default is ``false``). When the data is not strand-specific, a read is always
overlapping with a feature independently of whether maps to the same or the
opposite strand. For strand-specific data, the read has to be mapped to the
same strand as the feature.

``min`` defines the minimum amount of overlaps a given feature must have, at
least, to be kept (default: 0, i.e., keep all counts). If you just want to
discard features that are exactly zero, you should set the ``discard_zeros``
argument to True.

``normalization`` specifies if and how to normalize to take into account feature size:

- ``{raw}`` (default) is no normalization
- ``{normed}`` is the result of the ``{raw}`` mode divided by the size of the
  feature
- ``{scaled}`` is the result of the ``{normed}`` mode scaled up so that the
  total number of counts is identical to the ``{raw}`` (within rounding error)

Unmapped inserts are included in the output if ``{include_minus1}`` is true
(default: ``False``).


.. versionadded:: 0.6
    Before version 0.6, the default was to **not** include the -1 fraction.

substrim
--------

Given a read, returns another that is the biggest sub-sequence with a
given minimum quality. For example::

    read = substrim(read, min_quality=25)

Argument:
~~~~~~~~~

ShortRead

Return:
~~~~~~~

ShortRead

Arguments
~~~~~~~~~

+-------------------------+--------------+------------+----------------+
| Name                    | Type         | Required   | Default Value  |
+=========================+==============+============+================+
| min_quality             | Integer      |  yes       |	               |
+-------------------------+--------------+------------+----------------+

``min_quality`` parameter defines the minimum quality accepted for the
sub-sequence.

endstrim
--------

Given a read, trim from both ends (5' and 3') all bases below a minimal
quality. For example::

    read = endstrim(read, min_quality=25)

Argument:
~~~~~~~~~

ShortRead

Return:
~~~~~~~

ShortRead

Arguments
~~~~~~~~~

+-------------------------+--------------+------------+----------------+
| Name                    | Type         | Required   | Default Value  |
+=========================+==============+============+================+
| min_quality             | Integer      |  yes       |	               |
+-------------------------+--------------+------------+----------------+

``min_quality`` parameter defines the minimum quality value.

write
-----

Writes an object to disk.


ReadSet
~~~~~~~

Argument:
#########

Any

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
| format  | String      | no         | -              |
+---------+-------------+------------+----------------+

The argument ``ofile`` is where to write the content.

The output format is typically determined from the ``ofile`` extension, but the
``format`` argument overrides this. Supported formats:

- CountsTable: ``{tsv}`` (default) or ``{csv}``: use TAB or COMMA as a delimiter
- MappedReadSet: ``{sam}`` (default) or ``{bam}``
- ReadSet: FastQ format, optionally compressed (depending on the extension).

print
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

readlines
---------

Reads a text file and returns a list with all the strings in the file

Argumment
~~~~~~~~~

string: the filename

Example
~~~~~~~

``readlines`` is useful in combination with the `parallel
<stdlib.html#parallel-module>`__ module, where you can then use the ``lock1``
function to process a large set of inputs::

    sample = lock1(readlines('samplelist.txt'))


assemble
--------

`assemble`

Implementation
~~~~~~~~~~~~~~

`assemble()` uses the `MEGAHIT
<https://academic.oup.com/bioinformatics/article/31/10/1674/177884>`__
assembler.

Arguments
~~~~~~~~~

ReadSet

Returns
~~~~~~~

string : generated file

orf_find
--------

`orf_find` finds open reading frames (ORFs) in a sequence set::

    contigs = assemble(input)
    orfs = select(contigs, is_metagenome=True)

Argument:
~~~~~~~~~

SequenceSet

Return:
~~~~~~~

SequenceSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-----------------+-------------+------------+----------------+
| Name            | Type        | Required   | Default Value  |
+=================+=============+============+================+
| is_metagenome   | Bool        | yes        | -              |
+-----------------+-------------+------------+----------------+
| coords_out      | FilePath    | no         | -              |
+-----------------+-------------+------------+----------------+
| prots_out       | FilePath    | no         | -              |
+-----------------+-------------+------------+----------------+

Implementation
~~~~~~~~~~~~~~

NGLess uses `Prodigal
<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2848648/>`__ as the underlying
gene finder.

