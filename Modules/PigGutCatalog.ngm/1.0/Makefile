VERSION := 0.9
PACKAGE_CONTENTS := pig.fna pig.functional.map module.yaml

pig-gut-${VERSION}.tar.gz: ${PACKAGE_CONTENTS}
	tar -cz -f $@ --transform "s#^#Modules/PigGutCatalog.ngm/${VERSION}/#" $^

pig.functional.map:
	curl https://zenodo.org/record/1295720/files/pig_287sample.emapper.annotations | grep -v '^# ' > $@

pig.fna:
	curl ftp://parrot.genomics.cn/gigadb/pub/10.5524/100001_101000/100187/00.geneset/287sample_7.7M.GeneSet.fa.gz | gunzip > $@ 
