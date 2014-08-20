.. _Organisms:

Organisms
=============

Genome references available
---------------------------

NGLess provides archives containing data sets of organisms. Is also provided gene annotations that provide information 
about protein-coding and non-coding genes, splice variants, cDNA and protein sequences, non-coding RNAs.

The following table represents organisms provided by default:

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


Data Set Structure
--------------------
The archives provided by NGLess contain BWA index files, the genome reference file and a gene annotation file.

These archives are all created using version 75 of `Ensembl <http://www.ensembl.org/>`_.

::

 Description.tar.gz
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

The basename of Description.tar.gz (Description) will have the description name of the respective organism (i.e, Mus_musculus.tar.gz). 

Automatic installation
----------------------
It is possible to use the provided data sets directly on NGLess by simply typing its name. 
If it isn't already available locally (either on user or on root mode), then the archive is 
downloaded and the script execution is paused until the download is complete and the archive installed. 

The archive is installed on **user** mode and so at **home**/.ngless/genomes.

Manual installation
--------------------
Is possible to install data sets locally, before running any script. They can be installed locally in **user** mode or globally in **root** mode.

To install locally (organism bos taurus), is as simple as::

  $ ngless --install-reference-data bosTau4

And to install globally is::

  $ sudo ngless --install-reference-data bosTau4
 
When attempting to install an organism if is returned **True** it means that the organism is already installed, and there is no reason to install again. Otherwise, a progress bar is displayed to provide information on the download.
 
Note: Can be used flag --i instead of --install-reference-data.
