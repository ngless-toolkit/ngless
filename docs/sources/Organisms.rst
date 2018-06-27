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
<http://www.ensembl.org/>`__.

Automatic installation
----------------------

The builtin datasets are downloaded the first time they are used. They are
downloaded to the user home directory and stored in **home**/.ngless/genomes.

Manual installation
--------------------

Is possible to install data sets locally, before running any script. They can
be installed in **User** mode or in **Root** mode.

To install locally (organism bos taurus), use the following command::

  $ ngless --install-reference-data bosTau4

If you install as a super-user, then the dataset will be available for all
users::

  $ sudo ngless --install-reference-data bosTau4

When attempting to install an organism if is returned **True** it means that
the organism is already installed, and there is no reason to install again.
Otherwise, a progress bar is displayed to provide information on the download.

Data Set Structure
-------------------

This section provides the technical details necessary if you wish to build your
own reference for others to use automatically. For most users, it will likely
be easier to directly specify the references in the ngless script.

The archives provided by NGLess contain BWA index files, the genome reference
file and a gene annotation file.

::

 Name.tar.gz
  |
  |--- Sequence
  |     |
  |     |-- BWAIndex
  |             |-- genome.fa.gz
  |             |-- genome.fa.gz.amb
  |             |-- genome.fa.gz.ann
  |             |-- genome.fa.gz.bwt
  |             |-- genome.fa.gz.pac
  |             |-- genome.fa.gz.sa
  |
  |--- Annotation
        |-- annot.gtf.gz

The basename of Description.tar.gz (Description) will have the description name
of the respective organism (i.e, Mus_musculus.tar.gz).

