VERSION := 0.9
PACKAGE_CONTENTS := igc.fna igc.functional.map module.yaml

igc-${VERSION}.tar.gz: ${PACKAGE_CONTENTS}
	tar -cz -f $@ --transform "s#^#Modules/igc.ngm/${VERSION}/#" $^

igc.functional.map:
	curl https://zenodo.org/record/1299267/files/IGC_catalog.emapper.annotations | grep -v '^# ' > $@

igc.zip:
	wget http://vm-lux.embl.de/~kultima/share/MOCAT/v2.0/IGC.zip -O $@

igc.unzipped: igc.zip
	unzip $< IGC.1 IGC.2
	touch $@

igc.fna: igc.unzipped
	# IGC.2 is missing a line ending
	(cat IGC.1 IGC.2; echo) > $@
