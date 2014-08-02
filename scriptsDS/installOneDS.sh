#!/bin/bash

#version ensembl
version=75

#Human
homo_sapiens=Homo_sapiens.GRCh37

#Saccharomyces
saccharomyces_cerevisiae=Saccharomyces_cerevisiae.R64-1-1

#Caenorhabditis
caenorhabditis_elegans=Caenorhabditis_elegans.WBcel235

#Drosophila
drosophila_melanogaster=Drosophila_melanogaster.BDGP5

#Gallus
gallus_gallus=Gallus_gallus.Galgal4

#Canis
canis_familiaris=Canis_familiaris.CanFam3.1

#Taurus
bos_taurus=Bos_taurus.UMD3.1

#Rattus
rattus_norvegicus=Rattus_norvegicus.Rnor_5.0

#Musculus
mus_musculus=Mus_musculus.GRCm38

bwa_v=0.7.7
bwa=http://sourceforge.net/projects/bio-bwa/files/bwa-$bwa_v.tar.bz2/download

download(){
  echo "start downloading" $2
  wget -O $1 $2 &> /dev/null;
  r=$? 
  if [[ $r != 0 ]] ; then
    rm -rf $3
    echo "ERROR downloading " $2 "."
    exit 3 
  fi
  echo "ended downloading" $1
  
}

generateindex(){
  echo "start generating index"
  ../../../bwa-$bwa_v/bwa index "genome.fa.gz";
  r=$?
  if [[ $r != 0 ]] ; then
    cd ../../../ ;rm -rf $1;
    echo "ERROR in " $1 ". Deleting content."
    exit 3 
  fi
  cd ../../../;
  echo "generated index."

}

genomepath(){
  base_name=$1
  mkdir -p $2/Sequence/BWAIndex && cd $_; \
  echo "Start Downloading ${!base_name}.$version.dna.toplevel.fa.gz..."
  download genome.fa.gz  "ftp://ftp.ensembl.org/pub/release-75/fasta/$1/dna/${!base_name}.$version.dna.toplevel.fa.gz" $2
  echo "Completed ${!base_name}.$version.dna.toplevel.fa.gz..."
  echo "Start indexing..."
  generateindex $2
  echo "Completed indexing"
}

gtfpath(){
  base_name=$1
  mkdir -p $2/Annotation && cd $_; \
  echo "Start Downloading ${!base_name}.$version.gtf.gz..."
  download annot.gtf.gz "ftp://ftp.ensembl.org/pub/release-75/gtf/$1/${!base_name}.$version.gtf.gz" $2
  echo "Completed ${!base_name}.$version.gtf.gz"
  cd ../../;
}

installbwa(){
	if [ ! -d "bwa-$bwa_v" ]; then
  		download bwa-$bwa_v.tar.bz2 http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2/download $2
      tar -xvf bwa-$bwa_v.tar.bz2
  		echo "[BWA] installed"
  		cd bwa-$bwa_v; make; cd ..
  	else
  		echo "[BWA] Already installed"
	fi
}

#main
echo "Start to install $1"

# Create root directory
dir=$(echo "$1" | perl -ne 'print ucfirst')

installbwa

genomepath $1 $dir
gtfpath $1 $dir

tar -zcvf $dir.tar.gz $dir
