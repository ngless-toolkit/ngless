#!/bin/bash

ensembl_version=75

declare -A datasets

datasets=(
    [homo_sapiens]=Homo_sapiens.GRCh37
    [saccharomyces_cerevisiae]=Saccharomyces_cerevisiae.R64-1-1
    [caenorhabditis_elegans]=Caenorhabditis_elegans.WBcel235
    [drosophila_melanogaster]=Drosophila_melanogaster.BDGP5
    [gallus_gallus]=Gallus_gallus.Galgal4
    [canis_familiaris]=Canis_familiaris.CanFam3.1
    [bos_taurus]=Bos_taurus.UMD3.1
    [rattus_norvegicus]=Rattus_norvegicus.Rnor_5.0
    [mus_musculus]=Mus_musculus.GRCm38
)

for base_name in "${!datasets[@]}"; do 
    level="toplevel"
    if [[ ${base_name} = "homo_sapiens" || ${base_name} = "mus_musculus" ]]; then
        level="primary_assembly"
    fi
    g="http://ftp.ensembl.org/pub/release-75/fasta/${base_name}/dna/${datasets[$base_name]}.${ensembl_version}.dna.${level}.fa.gz"
    a="http://ftp.ensembl.org/pub/release-75/gtf/${base_name}/${datasets[$base_name]}.${ensembl_version}.gtf.gz"
    ngless --create-reference-pack ${base_name}.tar.gz -g $g -a $a
done
