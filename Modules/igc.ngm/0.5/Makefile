VERSION := 0.5
PACKAGE_CONTENTS := IGC.fna IGC.functional.map module.yaml

igc-${VERSION}.tar.gz: ${PACKAGE_CONTENTS}
	tar -cz -f $@ --transform "s#^#Modules/igc.ngm/0.5/#" $^

IGC.zip:
	wget http://vm-lux.embl.de/~kultima/share/MOCAT/v2.0/IGC.zip

IGC.unzipped: IGC.zip
	unzip $<
	touch $@

IGC.fna: IGC.unzipped
	# IGC.2 is missing a line ending
	(cat IGC.1 IGC.2; echo) > $@

IGC.functional.map: IGC.unzipped
	ln IGC.1-2.functional.map $@

