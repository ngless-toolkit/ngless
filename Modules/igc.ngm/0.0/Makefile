all: data/IGC.fna data/IGC.functional.map

IGC.zip:
	wget http://vm-lux.embl.de/~kultima/share/MOCAT/v2.0/IGC.zip

IGC.unzipped: IGC.zip
	unzip $<
	touch $@

data/IGC.fna: IGC.unzipped
	mkdir -p data
	# IGC.2 is missing a line ending
	(cat IGC.1 IGC.2; echo) > $@

data/IGC.functional.map: IGC.unzipped
	mkdir -p data
	cp IGC.1-2.functional.map $@

