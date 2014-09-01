#!/bin/bash

dirname=HTSeq-0.6.1
fname=$dirname.tar.gz

url=http://pypi.python.org/packages/source/H/HTSeq/$fname#md5=b7f4f38a9f4278b9b7f948d1efbc1f05

isNotCompleted () {
  echo "File: $1 has $2 lines."
  if [ -f $1 ]; then
     ! fileSizeCorrect $1 $2
    else
    return 0
  fi
}

fileSizeCorrect(){
    v=$(wc -l < $1)
    test $v -eq $2
}

#remove last 5 lines of htseq, which has statistics.
filterEndL (){
  sed -n -e :a -e "1,$1!{P;N;D;};N;ba" $2
}

download(){
  echo "start downloading" $2
  wget -O $1 $2 &>/dev/null
  r=$?
  if [ $r != 0 ] ; then
    rm -rf $1
    echo "ERROR downloading " $2 "."
    exit 3
  fi
  echo "ended downloading" $1
}


downloadURL(){
    if [ ! -d "$dirname" ]; then
        download $fname $1
        echo tar -xzvf $fname
        tar -xzvf $fname
        cd $dirname; python setup.py install --user;
        if [ $r != 0 ]; then
            rm -rf $1
            echo "ERROR installing HTSEQ."
            exit 3
        fi
        cd ..
        echo "[HTSEQ] installed"
    else
        echo "[HTSEQ] Already installed"
    fi
    rm -rf $fname
}

downloadURL $url

# -a 0 allows to ignore all htseq filter on quality.
# Test features
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m union | filterEndL 5 > htseq_gene_noStrand_union.txt
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t exon -m union | filterEndL 5 > htseq_exon_noStrand_union.txt
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t CDS  -m union | filterEndL 5 >  htseq_cds_noStrand_union.txt

# Test mode
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m intersection-strict   | filterEndL 5 > htseq_gene_noStrand_inters-strict.txt
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m intersection-nonempty | filterEndL 5 > htseq_gene_noStrand_inters-nempty.txt

# Test strand Positive. Negative tested before
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s yes -t gene -m union | filterEndL 5 > htseq_gene_yesStrand_union.txt

python -m HTSeq.scripts.count ../sample.sam ../short.gtf -a 0 -s yes -t gene -m union | filterEndL 5 > htseq_gene_yesStrand_union_short.txt

exit 0
