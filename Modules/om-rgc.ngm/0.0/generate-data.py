import gzip

def empty(tok):
    if tok == '""': return ''
    return tok
with open('data/OM-RGC.fna', 'wt') as fa_out, open('data/OM-RGC.functional.map', 'wt') as map_out:
    map_out.write("#gene\teggNOG_OG\tKEGG_ko\tKEGG_module\n")
    for i,line in enumerate(gzip.open('./OM-RGC_seq.release.tsv.gz', 'rt')):
        if i == 0: continue
        tokens = line.split('\t')
        fa_out.write(">{}\n".format(tokens[0]))
        fa_out.write(tokens[12])
        map_out.write("{gene}\t{eggNOG}\t{ko}\t{module}\n".format(gene=tokens[0],
                            eggNOG=empty(tokens[2]),
                            ko=empty(tokens[3]),
                            module=empty(tokens[4])))
