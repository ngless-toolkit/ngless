datasets = [
    (75, 'bos_taurus', "Bos_taurus.UMD3.1", "bosTau4", "v1.0"),
    (75, 'caenorhabditis_elegans', "Caenorhabditis_elegans.WBcel235", "ce10", "v1.0"),
    (75, 'canis_familiaris', "Canis_familiaris.CanFam3.1", "canFam3", "v1.0"),
    (90, 'drosophila_melanogaster', "Drosophila_melanogaster.BDGP6", "dm6", "v1.0"),
    (75, 'drosophila_melanogaster', "Drosophila_melanogaster.BDGP5", "dm5", "v1.0"),
    (90, 'gallus_gallus', "Gallus_gallus.Gallus_gallus-5.0", "gg5", "v1.0"),
    (75, 'gallus_gallus', "Gallus_gallus.Galgal4", "gg4", "v1.0"),
    # NOTE on 25/11/2017 - both hg38 and mm10 were replaced by their specific
    # patch versions (hg38.p7 and mm10.p2, respectively), according to their
    # Assembly accession code. More info on the NOTE below
    (90, 'homo_sapiens', "Homo_sapiens.GRCh38", "hg38.p10", "v1.0"),
    (85, 'homo_sapiens', "Homo_sapiens.GRCh38", "hg38.p7", "v1.0"),
    (75, 'homo_sapiens', "Homo_sapiens.GRCh37", "hg19", "v1.0"),
    (90, 'mus_musculus', "Mus_musculus.GRCm38", "mm10.p5", "v1.0"),
    (75, 'mus_musculus', "Mus_musculus.GRCm38", "mm10.p2", "v1.0"),
    (90, 'rattus_norvegicus', "Rattus_norvegicus.Rnor_6.0", "rn6", "v1.0"),
    (75, 'rattus_norvegicus', "Rattus_norvegicus.Rnor_5.0", "rn5", "v1.0"),
    (75, 'saccharomyces_cerevisiae', "Saccharomyces_cerevisiae.R64-1-1", "sacCer3", "v1.0"),
    (90, 'sus_scrofa', "Sus_scrofa.Sscrofa11.1", "susScr11", "v1.0"),
]

# NOTE: Ensembl gives the same "release name" to different sub-releases or patches
# For instance GRCh38 is not specific enough. In release 85 it corresponded to
# patch 7 (p7) where release 90 included patch 10 (p10).
# To know what patch or sub-version is included on a release check the README
# on the download folder (example):
#   ftp://ftp.ensembl.org/pub/release-90/fasta/homo_sapiens/dna/README
# and the Assembly ID mentioned there.

for (ensembl_version, base_name, build, outputname, our_version) in datasets:
    level = "toplevel"
    if base_name in ["homo_sapiens", "mus_musculus"]:
        level = "primary_assembly"

    a = "http://ftp.ensembl.org/pub/release-{ensembl_version}/gtf/{base_name}/{build}.{ensembl_version}.gtf.gz".format(**locals())

    if ensembl_version <= 75:
        g = "http://ftp.ensembl.org/pub/release-{ensembl_version}/fasta/{base_name}/dna/{build}.{ensembl_version}.dna.{level}.fa.gz".format(**locals())
    else:
        g = "http://ftp.ensembl.org/pub/release-{ensembl_version}/fasta/{base_name}/dna/{build}.dna.{level}.fa.gz".format(**locals())

    outname = "{outputname}-{our_version}".format(**locals())
    print("ngless --create-reference-pack --output-name {outname}.tar.gz --genome-url {g} --gtf-url {a}".format(outname=outname, g=g, a=a))
