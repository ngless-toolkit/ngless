#/bin/sh

dirname=HTSeq-0.6.1
fname=$dirname.tar.gz

url=http://pypi.python.org/packages/source/H/HTSeq/$fname#md5=b7f4f38a9f4278b9b7f948d1efbc1f05

download(){
  echo "start downloading" $2
  wget -O $1 $2 &> /dev/null;
  r=$? 
  if [[ $r != 0 ]] ; then
    rm -rf $1
    echo "ERROR downloading " $2 "."
    exit 3 
  fi
  echo "ended downloading" $1
}



downloadURL(){
	if [ ! -d "$dirname" ]; then
  		download $fname $1
      	tar -xzvf $fname
  		cd $dirname; python setup.py install --user; cd ..
  		echo "[HTSEQ] installed"
  	else
  		echo "[HTSEQ] Already installed"
	fi
	rm -rf $fname
}

downloadURL $url

# -a 0 allows to ignore all htseq filter on quality.
# Test features
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m union > htseq_gene_noStrand_union.txt 
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t exon -m union > htseq_exon_noStrand_union.txt 
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t CDS  -m union >  htseq_cds_noStrand_union.txt 

# Test mode
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m intersection-strict   > htseq_gene_noStrand_inters-strict.txt 
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s no -t gene -m intersection-nonempty > htseq_gene_noStrand_inters-nempty.txt 

# Test strand Positive. Negative tested before
python -m HTSeq.scripts.count ../sample.sam ../sample.gtf -a 0 -s yes -t gene -m union > htseq_gene_yesStrand_union.txt 