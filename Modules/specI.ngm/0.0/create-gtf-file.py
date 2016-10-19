#!/usr/bin/env python
# converts data in MOCAT .coods & map files to the standard GTF format

from collections import defaultdict
coord_file = 'data/RefMG.v1.padded.coord'
refmg_map_file = 'data/RefMG.v1.padded.refmg.map'
RANKS = ['kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'specI_cluster',]

coords = {}
for line in open(coord_file):
    gene,start,end = line.strip().split()
    coords[gene] = (start,end)

genes = [line[1:].split()[0] for line in open('data/RefMG.v1.padded') if line[0] =='>']
taxa2genes = defaultdict(list)
for g in genes:
    t = g.split('.')[0]
    taxa2genes[t].append(g)

for line in open(refmg_map_file):
    tokens = line.rstrip().split('\t')
    for gene in taxa2genes[tokens[0]]:
        start,end = coords[gene]
        for r,tk in zip(RANKS, tokens[1:]):
            print("\t".join([
                    gene,
                    "specI",
                    r,
                    start,
                    end,
                    ".",
                    ".",
                    ".",
                    "ID \"{}\"".format(tk)]))

