.. _Organisms:

Available Reference Genomes
===========================

NGLess provides builtin support for the most widely used model organisms
(human, mouse, yeast, C. elegans, ...; see the full table below). This makes it
easier to use the tool when using these organisms as some knowledge is already
built in.

Genome references available
---------------------------

NGLess provides archives containing data sets of organisms. Is also provided
gene annotations that provide information about protein-coding and non-coding
genes, splice variants, cDNA and protein sequences, non-coding RNAs.

The following table represents organisms provided by default:

+-----------+-----------------------------+-------------------+---------+
| Name      | Description                 | Assembly          | Ensembl |
+===========+=============================+===================+=========+
| bosTau4   | bos\_taurus                 | UMD3.1            | 75      |
+-----------+-----------------------------+-------------------+---------+
| ce10      | caenorhabditis\_elegans     | WBcel235          | 75      |
+-----------+-----------------------------+-------------------+---------+
| canFam3   | canis\_familiaris           | CanFam3.1         | 75      |
+-----------+-----------------------------+-------------------+---------+
| dm6       | drosophila\_melanogaster    | BDGP6             | 90      |
+-----------+-----------------------------+-------------------+---------+
| dm5       | drosophila\_melanogaster    | BDGP5             | 75      |
+-----------+-----------------------------+-------------------+---------+
| gg5       | gallus_gallus               | Gallus_gallus-5.0 | 90      |
+-----------+-----------------------------+-------------------+---------+
| gg4       | gallus_gallus               | GalGal4           | 75      |
+-----------+-----------------------------+-------------------+---------+
| hg38.p10  | homo\_sapiens               | GRCh38.p10        | 90      |
+-----------+-----------------------------+-------------------+---------+
| hg38.p7   | homo\_sapiens               | GRCh38.p7         | 85      |
+-----------+-----------------------------+-------------------+---------+
| hg19      | homo\_sapiens               | GRCh37            | 75      |
+-----------+-----------------------------+-------------------+---------+
| mm10.p5   | mus\_musculus               | GRCm38.p5         | 90      |
+-----------+-----------------------------+-------------------+---------+
| mm10.p2   | mus\_musculus               | GRCm38.p2         | 75      |
+-----------+-----------------------------+-------------------+---------+
| rn6       | rattus\_norvegicus          | Rnor\_6.0         | 90      |
+-----------+-----------------------------+-------------------+---------+
| rn5       | rattus\_norvegicus          | Rnor\_5.0         | 75      |
+-----------+-----------------------------+-------------------+---------+
| sacCer3   | saccharomyces\_cerevisiae   | R64-1-1           | 75      |
+-----------+-----------------------------+-------------------+---------+
| susScr11  | sus\_scrofa                 | Sscrofa11.1       | 90      |
+-----------+-----------------------------+-------------------+---------+

These archives are all created using versions 75, 85 and 90 of `Ensembl
<https://www.ensembl.org/>`__.

Automatic installation
----------------------

The builtin datasets are downloaded the first time they are used and stored in
a ``References`` subdirectory of the NGLess data directory:

- the global data directory (``<prefix>/share/ngless/data``, where ``<prefix>``
  is the directory containing ``bin/ngless``; this is normally the case for a
  conda or pixi install) if it is writable, so that the data is shared by
  everyone using that installation;
- otherwise, the user data directory (by default,
  ``$HOME/.local/share/ngless/data``).

Both locations can be changed in the `configuration <configuration.html>`__
(``global-data-directory`` and ``user-data-directory``).

Manual installation
--------------------

It is possible to install data sets before running any script (e.g., on a
machine with network access, before running on compute nodes that do not have
it). For example, to install the bos taurus reference, use the following
command::

  $ ngless --install-reference-data bosTau4

This uses the same location rules as above: if the global data directory is
writable (e.g., when running as the user who owns the installation, or with
``sudo``), the dataset will be available for all users of that installation.

If the reference is already installed, nothing is downloaded. Otherwise, a
progress bar is displayed while downloading.
