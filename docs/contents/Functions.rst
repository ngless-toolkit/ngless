.. _Functions:

=========
Functions
=========

Fastq
-----

Function to load, one or more, fastQ files. An example::

  in = fastq('input.fq')

Argument:
~~~~~~~~~
NGOString

Return:
~~~~~~~
NGOReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~
none

The only compression method supported for the data sets is **gzip** (.gz).

The encoding prediction is built on the lowest ASCII character of the fastQ file.

When loading a data set, quality control is carried out and statistics can be visualised in a graphical user interface (GUI).

Simple statistics calculated are percentage of guanine and cytosine (%GC), encoding, number of sequences and minimum maximum sequence length. The more complex statistics calculated are the mean, median, lower quartile and upper quartile for each position of the base pairs.


Unique
------

Function that given a set of reads, returns another which only retains a
set number of copies of each read (if there are any duplicates). An
example:

::

    input = unique(input, max_copies=3)

Argument:
~~~~~~~~~

NGOReadSet

Return:
~~~~~~~

NGOReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+---------------+--------------+------------+
| Name          | Type         | Required   |
+===============+==============+============+
| max\_copies   | NGOInteger   |  no        |
+---------------+--------------+------------+

The optional argument **max_copies** allows to define the number of tolerated copies (default: 2).

Is considered a copy: NGOShortReads with the same sequence regardless
of quality and identifier.

Preprocess
----------

This function executes the given block for each read in the NGOReadSet.
Unless the read is **discarded**, it is transferred (after
transformations) to the output. The output is assigned to the same name
as the inputs. An example:

::

    preprocess(inputs) using |read|:
        read = read[3:]

Argument:
~~~~~~~~~

NGOReadSet

Return:
~~~~~~~

NGOVoid

Arguments by value:
~~~~~~~~~~~~~~~~~~~
none

This function also performs quality control on its output.



Map
---

The function map, maps a NGOReadSet to reference. An example:

::

    mapped = map(input,reference='sacCer3')

Argument:
~~~~~~~~~

NGOReadSet

Return:
~~~~~~~

NGOMappedReadSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------+-------------+------------+
| Name        | Type        | Required   |
+=============+=============+============+
| reference   | NGOString   | yes        |
+-------------+-------------+------------+

The argument **reference** can either be a path to a data set or the
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

The argument **reference** can either be a path to a data set or the **name** of a NGLess provided data set. Provided data sets of NGLess are:


Annotate
--------

Given a file with aligned sequencing reads (NGOReadSet) and a list of
genomic features (gff file), the function allows to annotate reads to
each feature. An example:

::

    annotated = annotate(mapped, strand={no}, mode="union", ambiguity={allow})

Argument:
~~~~~~~~~

NGOMappedReadSet

Return:
~~~~~~~

NGOAnnotatedSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------+-----------------+------------+----------------+
| Name        | Type            | Required   | Default value  |
+=============+=================+============+================+
| gff         | NGOString       | yes*       |  -             |
+-------------+-----------------+------------+----------------+
| features    | [ NGOSymbol ]   | no         | {gene}         |
+-------------+-----------------+------------+----------------+
| mode        | NGOString       | no         | {union}        |
+-------------+-----------------+------------+----------------+
| ambiguity   | NGOSymbol       | no         | {allow}        |
+-------------+-----------------+------------+----------------+
| strand      | NGOSymbol       | no         | {no}           |
+-------------+-----------------+------------+----------------+


The **gff** argument is required, unless a known reference was used for mapping.

**features** represents which features to keep, discarding everything else. If
nothing is provided, everything is considered to be significant. Possible
symbols are **{gene}**, **{exon}**, and **{cds}**.

**Mode** is a symbol which dictates how to handle reads overlapping more than
one feature. Possible values for ``mode`` are ``union``,
``intersection-strict`` and ``intersection-nonempty`` (default: ``union``).
For each read position are obtained features that intersect it, which is known
as sets. The different modes are:

-  ``union`` the union of all the sets.

-  ``intersection-strict`` the intersection of all the sets.

-  ``intersection-nonempty`` the intersection of all non-empty sets.

The ``ambiguity`` argument is an opportunity to decide whether to annotate
reads that overlap with more than one feature. Possible values are ``{allow}``
and ``{deny}`` (default: ``{allow}``).

Argument ``strand`` represents whether the data are from a strand-specific and
the possible values can be **{yes}** or **{no}** (default: {no}). For {no}, a
read is always overlapping with a feature independently of whether maps to the
same or the opposite strand. For {yes}, the read has to be mapped to the same
strand as the feature.



Count
-----

Function that allows to filter the counts of features. Example:

::

    counts = count(annotated, min=2)

Argument:
~~~~~~~~~

NGOAnnotatedSet

Return:
~~~~~~~

NGOAnnotatedSet

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+----------+-----------------+------------+
| Name     | Type            | Required   |
+==========+=================+============+
| counts   | [ NGOSymbol ]   |  no        |
+----------+-----------------+------------+
| min      | NGOInteger      |  no        |
+----------+-----------------+------------+

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

NGOShortRead

Return:
~~~~~~~

NGOShortRead

Arguments by value:
~~~~~~~~~~~~~~~~~~~

+-------------------------+--------------+------------+
| Name                    | Type         | Required   |
+=========================+==============+============+
| min_quality             | NGOInteger   |  no        |
+-------------------------+--------------+------------+

**Min_quality** parameter defines the minimum quality
accepted for the sub-sequence (default: 0).

Write
-----

Write function allows to write a NGLessObject to Disk. Different Types
of NGLessObject are manipulated in different manners.


NGOReadSet
~~~~~~~~~~~

Argument:
##########

NGOReadSet

Return:
##########

NGOVoid

Arguments by value:
###################

+---------+-------------+------------+
| Name    | Type        | Required   |
+=========+=============+============+
| ofile   | NGOString   | yes        |
+---------+-------------+------------+

The argument **ofile** is a file path to where the content is written.

NGOMappedReadSet
~~~~~~~~~~~~~~~~~

Argument:
##########

NGOMappedReadSet

Return:
##########

NGOVoid

Arguments by value:
###################

+----------+-------------+------------+
| Name     | Type        | Required   |
+==========+=============+============+
| ofile    | NGOString   |  yes       |
+----------+-------------+------------+
| format   | NGOString   |  no        |
+----------+-------------+------------+

**Format** can have value **{bam}** or **{sam}** (default: {sam}).

NGOAnnotatedSet
~~~~~~~~~~~~~~~

Argument:
##########

NGOAnnotatedSet

Return:
##########

NGOVoid

Arguments by value:
###################

+----------+-------------+------------+
| Name     | Type        | Required   |
+==========+=============+============+
| ofile    | NGOString   |  yes       |
+----------+-------------+------------+
| format   | NGOString   |  no        |
+----------+-------------+------------+

**Format** can have value **{csv}** or **{tsv}** (default: {tsv}).

If a list of **any** of the previously mentioned data types is provided, the **ofile** argument must use an **{index}** in the template name to differentiate between the files in the list. For example for a list with two elements:

::

    ofile = "../samples/CountsResult{index}.txt"

| would result in,

** “../samples/CountsResult1.txt”, “../samples/CountsResult2.txt” **

Print
-----

Print function allows to print a NGLessObject to IO.

Argument:
~~~~~~~~~
NGLessObject

Return:
~~~~~~~
NGOVoid

Arguments by value:
~~~~~~~~~~~~~~~~~~~
none
