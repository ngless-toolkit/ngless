.. _Organisms:

Available Reference Genomes
===========================

NGLess provides builtin support for the most widely used model organisms
(human, mouse, yeast, C. elegans, ...; see the full table below). To facilitate 

Genome references available
---------------------------

NGLess provides archives containing data sets of organisms. Is also provided
gene annotations that provide information about protein-coding and non-coding
genes, splice variants, cDNA and protein sequences, non-coding RNAs.

The following table represents organisms provided by default:

+-----------+-----------------------------+-------------+
| Name      | Description                 | Assembly    |
+===========+=============================+=============+
| hg19      | homo\_sapiens               | GRCh38      |
+-----------+-----------------------------+-------------+
| sacCer3   | saccharomyces\_cerevisiae   | R64-1-1     |
+-----------+-----------------------------+-------------+
| ce10      | caenorhabditis\_elegans     | WBcel235    |
+-----------+-----------------------------+-------------+
| dm3       | drosophila\_melanogaster    | BDGP5       |
+-----------+-----------------------------+-------------+
| canFam2   | canis\_familiaris           | CanFam3.1   |
+-----------+-----------------------------+-------------+
| rn4       | rattus\_norvegicus          | Rnor\_5.0   |
+-----------+-----------------------------+-------------+
| bosTau4   | bos\_taurus                 | UMD3.1      |
+-----------+-----------------------------+-------------+
| mm10      | mus\_musculus               | GRCm38      |
+-----------+-----------------------------+-------------+

These archives are all created using version 75 of `Ensembl
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

