VERSION := 0.9
PACKAGE_CONTENTS := mouse.fna mouse.functional.map module.yaml

mouse-gut-${VERSION}.tar.gz: ${PACKAGE_CONTENTS}
	tar -cz -f $@ --transform "s#^#Modules/MouseGutCatalog.ngm/${VERSION}/#" $^

mouse.functional.map:
	curl https://zenodo.org/record/1295720/files/mouse_184samples.emapper.annotations | grep -v '^# ' > $@

mouse.fna:
	curl ftp://penguin.genomics.cn/pub/10.5524/100001_101000/100114/Genecatalog/184sample_2.6M.GeneSet.fa.gz | gunzip > $@
