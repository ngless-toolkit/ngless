VERSION := 0.9
PACKAGE_CONTENTS := dog.fna dog.functional.map module.yaml

dog-gut-${VERSION}.tar.gz: ${PACKAGE_CONTENTS}
	tar -cz -f $@ --transform "s#^#Modules/DogGutCatalog.ngm/${VERSION}/#" $^

dog.functional.map:
	curl https://zenodo.org/record/1295720/files/dogs.129.emapper.annotations | grep -v '^# ' > $@

dog.fna:
	curl https://zenodo.org/record/1198434/files/dogs.129.padded.fna.xz | unxz > $@
