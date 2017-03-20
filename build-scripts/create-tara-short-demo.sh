#!/usr/bin/env bash

set -e

mkdir -p SAMEA2621229
ngless --download-file --download-url=http://ftp.sra.ebi.ac.uk/vol1/fastq/ERR594/ERR594355/ERR594355_1.fastq.gz --local-file=SAMEA2621229/ERR594355_1.fastq.gz
ngless --download-file --download-url=http://ftp.sra.ebi.ac.uk/vol1/fastq/ERR594/ERR594355/ERR594355_2.fastq.gz --local-file=SAMEA2621229/ERR594355_2.fastq.gz

mkdir -p SAMEA2621155
ngless --download-file --download-url=http://ftp.sra.ebi.ac.uk/vol1/fastq/ERR599/ERR599133/ERR599133_1.fastq.gz --local-file=SAMEA2621155/ERR599133_1.fastq.gz
ngless --download-file --download-url=http://ftp.sra.ebi.ac.uk/vol1/fastq/ERR599/ERR599133/ERR599133_2.fastq.gz --local-file=SAMEA2621155/ERR599133_2.fastq.gz


mkdir -p SAMEA2621033
ngless --download-file --download-url=http://ftp.sra.ebi.ac.uk/vol1/fastq/ERR594/ERR594391/ERR594391_1.fastq.gz --local-file=SAMEA2621033/ERR594391_1.fastq.gz
ngless --download-file --download-url=http://ftp.sra.ebi.ac.uk/vol1/fastq/ERR594/ERR594391/ERR594391_2.fastq.gz --local-file=SAMEA2621033/ERR594391_2.fastq.gz

echo SAMEA2621229 >> tara.demo
echo SAMEA2621155 >> tara.demo
echo SAMEA2621033 >> tara.demo
