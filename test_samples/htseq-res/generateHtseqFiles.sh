#!/usr/bin/env bash

set -e

PYTHON=python

#remove last 5 lines of htseq, which has statistics.
filterEndL (){
  sed -n -e :a -e "1,$1!{P;N;D;};N;ba" $2
}

# -a 0 allows to ignore all htseq filter on quality.
# Test features
$PYTHON -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m union | filterEndL 5 > htseq_gene_noStrand_union.txt
$PYTHON -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t exon -m union | filterEndL 5 > htseq_exon_noStrand_union.txt
$PYTHON -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t CDS  -m union | filterEndL 5 >  htseq_cds_noStrand_union.txt

# Test mode
$PYTHON -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m intersection-strict   | filterEndL 5 > htseq_gene_noStrand_inters-strict.txt
$PYTHON -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m intersection-nonempty | filterEndL 5 > htseq_gene_noStrand_inters-nempty.txt

# Test strand Positive. Negative tested before
$PYTHON -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s yes -t gene -m union | filterEndL 5 > htseq_gene_yesStrand_union.txt
$PYTHON -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s yes -t gene -m intersection-nonempty | filterEndL 5 > htseq_gene_yesStrand_nempty.txt

$PYTHON -m HTSeq.scripts.count ../sample.sam ../short.gtf -a 0 -s yes -t gene -m union | filterEndL 5 > htseq_gene_yesStrand_union_short.txt

# Very short tests (regression tests)
$PYTHON -m HTSeq.scripts.count ../very_short.sam ../very_short.gtf -a 0 -s yes -t gene -m union | filterEndL 5 > htseq_gene_yesStrand_union_very_short.txt

$PYTHON -m HTSeq.scripts.count ../very_short.sam ../very_short.gtf -a 0 -s yes -t gene -m intersection-nonempty | filterEndL 5 > htseq_gene_yesStrand_nempty_very_short.txt
$PYTHON -m HTSeq.scripts.count ../very_short.sam ../very_short.gtf -a 0 -s no -t gene -m intersection-nonempty | filterEndL 5 > htseq_gene_noStrand_nempty_very_short.txt

$PYTHON -m HTSeq.scripts.count ../nonempty_diff.sam ../nonempty_diff.gtf -a 0 -s no -t gene -m intersection-nonempty | filterEndL 5 > htseq_gene_noStrand_inters-nempty_diff.txt

exit 0
