#!/bin/bash

ds=("saccharomyces_cerevisiae" "caenorhabditis_elegans" "drosophila_melanogaster" "gallus_gallus" "canis_familiaris" "rattus_norvegicus" "bos_taurus" "mus_musculus")

for i in "${ds[@]}"
do
	echo "Install $i"
	./installOneDS.sh $i
done
