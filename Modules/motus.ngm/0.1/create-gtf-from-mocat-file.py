#!/usr/bin/env python

# converts data in MOCAT .coods & map files to the standard GTF format

# Load coordinates
coords = {}
for line in open('data/mOTU.v1.padded.coord'):
    gene,start,end = line.strip().split()
    coords[gene] = (start,end)

# Each line in the map file becomes on GTF line
for line in open('data/mOTU.v1.padded.motu.map'):
    gene, _, cog, cog_cluster_id = line.strip().split('\t')
    start,end = coords[gene]
    print("\t".join([
            gene,
            "mOTU",
            "gene",
            start,
            end,
            ".",
            ".",
            ".",
            "gene_id \"{}.{}\"".format(cog, cog_cluster_id)]))

