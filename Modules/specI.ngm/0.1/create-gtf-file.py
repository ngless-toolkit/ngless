#!/usr/bin/env python
# converts data in MOCAT .coods & map files to the standard GTF format

from collections import defaultdict
def norm_undef(tokens):
    valid = 0
    ntokens = []
    for i,tk in enumerate(tokens):
        if tk == 'undef':
            ntokens.append('unclassified ' + tokens[valid])
        else:
            ntokens.append(tk)
            valid = i
    return ntokens

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

taxa2genes = dict(taxa2genes)

for line in open(refmg_map_file):
    tokens = line.rstrip().split('\t')
    tokens = norm_undef(tokens)
    for gene in taxa2genes.get(tokens[0], ['missing']):
        if gene == 'missing':
            start, end = '0', '1000'
        else:
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

