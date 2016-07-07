ensembl_version="75"

datasets = [
    ('homo_sapiens', "Homo_sapiens.GRCh37", "hg19"),
    ('saccharomyces_cerevisiae', "Saccharomyces_cerevisiae.R64-1-1", "sacCer3"),
    ('caenorhabditis_elegans', "Caenorhabditis_elegans.WBcel235", "ce10"),
    ('drosophila_melanogaster', "Drosophila_melanogaster.BDGP5", "dm3"),
    ('gallus_gallus', "Gallus_gallus.Galgal4", "gg4"),
    ('canis_familiaris', "Canis_familiaris.CanFam3.1", "cm3"),
    ('bos_taurus', "Bos_taurus.UMD3.1", "bosTau4"),
    ('rattus_norvegicus', "Rattus_norvegicus.Rnor_5.0", "rn4"),
    ('mus_musculus', "Mus_musculus.GRCm38", "mm10"),
]

for (base_name,build, outputname) in datasets:
    level="toplevel"
    if base_name in ["homo_sapiens", "mus_musculus"]:
        level="primary_assembly"

    g="http://ftp.ensembl.org/pub/release-{ensembl_version}/fasta/{base_name}/dna/{build}.{ensembl_version}.dna.{level}.fa.gz".format(**locals())
    a="http://ftp.ensembl.org/pub/release-{ensembl_version}/gtf/{base_name}/{build}.{ensembl_version}.gtf.gz".format(**locals())
    print("ngless --create-reference-pack --output-name {outputname}.tar.gz --genome-url {g} --gtf-url {a}".format(outputname=outputname, g=g, a=a))
