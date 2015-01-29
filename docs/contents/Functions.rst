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
+---------------+--------------+------------+----------------+
| Name          | Type         | Required   | Default Value  |
+===============+==============+============+================+
| encoding      | Symbol       |  no        | {auto}         |
+---------------+--------------+------------+----------------+

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

The only compression method supported for the data sets is **gzip** (.gz).


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
none

This function also performs quality control on its output.



Map
---

The function map, maps a ReadSet to reference. For example::

    mapped = map(input, reference='sacCer3')

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
| reference   | String      | yes        | -              |
+-------------+-------------+------------+----------------+

The argument ``reference`` can either be a file path to a FASTA file or the
name of a builtin dataset. NGLess provides the following builtin datasets:

+-----------+-----------------------------+-------------+
| Name      | Description                 | Assembly    |
+===========+=============================+=============+
| sacCer3   | saccharomyces\_cerevisiae   | R64-1-1     |
+-----------+-----------------------------+-------------+
| ce10      | caenorhabditis\_elegans     | WBcel235    |
+-----------+-----------------------------+-------------+
| dm3       | drosophila\_melanogaster    | BDGP5       |
+-----------+-----------------------------+-------------+
| `-`       | gallus\_gallus              | Galgal4     |
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


Annotate
--------

Given a file with aligned sequencing reads (ReadSet) and a list of
genomic features (gff file), the function allows to annotate reads to
each feature. For example::

    annotated = annotate(mapped, strand=false, mode={union}, keep_ambiguous=false)

Argument:
~~~~~~~~~

MappedReadSet

Return:
~~~~~~~

AnnotatedSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------------+-----------------+------------+----------------+
| Name              | Type            | Required   | Default value  |
+===================+=================+============+================+
| gff               | String          | yes*       |  -             |
+-------------------+-----------------+------------+----------------+
| features          | [ Symbol ]      | no         | {gene}         |
+-------------------+-----------------+------------+----------------+
| mode              | Symbol          | no         | {union}        |
+-------------------+-----------------+------------+----------------+
| keep\_ambiguous   | Bool            | no         | true           |
+-------------------+-----------------+------------+----------------+
| strand            | Bool            | no         | false          |
+-------------------+-----------------+------------+----------------+


The ``gff`` argument is required, unless a known reference was used for mapping.

**features** represents which features to keep, discarding everything else.
Possible symbols are ``{gene}``, ``{exon}``, and ``{cds}``.

**Mode** is a symbol which dictates how to handle reads overlapping more than
one feature. Possible values for ``mode`` are ``{union}``,
``{intersection-strict}``, and ``{intersection-nonempty}`` (default:
``{union}``). For each read position are obtained features that intersect it,
which is known as sets. The different modes are:

-  ``{union}`` the union of all the sets.

-  ``{intersection-strict}`` the intersection of all the sets.

-  ``{intersection-nonempty}`` the intersection of all non-empty sets.

The ``keep_ambiguous`` argument is an opportunity to decide whether to annotate
reads that overlap with more than one feature.

Argument ``strand`` represents whether the data are from a strand-specific
(default is ``false``). When the data is not strand-specific, a read is always
overlapping with a feature independently of whether maps to the same or the
opposite strand. For strand-specific data, the read has to be mapped to the
same strand as the feature.



Count
-----

Function that allows to filter the counts of features. Example::

    counts = count(annotated, min=2)

Argument:
~~~~~~~~~

AnnotatedSet

Return:
~~~~~~~

AnnotatedSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+----------+-----------------+------------+----------------+
| Name     | Type            | Required   | Default Value  |
+==========+=================+============+================+
| counts   | [ Symbol ]      |  no        | -              |
+----------+-----------------+------------+----------------+
| min      | Integer         |  no        | 0              |
+----------+-----------------+------------+----------------+

The argument **counts** represents which features to keep, discarding everything else. Possible symbols are gene, exon and cds. If nothing is provided everything is considered to be important.

**Min** defines the minimum amount of overlaps a given feature must have, at least, to be kept (default: 0).


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

AnnotatedSet
~~~~~~~~~~~~~~~

Argument:
##########

AnnotatedSet

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
