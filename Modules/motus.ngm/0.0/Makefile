all: data/motus.gtf.gz data/mOTU.v1.padded data/mOTU.v1.padded.motu.linkage.map

data/motus.gtf.gz: data/mOTU.v1.padded.coord data/mOTU.v1.padded.motu.map
	python create-gtf-from-mocat-file.py | gzip > $@

data/mOTU.v1.padded.coord: data/mOTU.v1.padded
data/mOTU.v1.padded.motu.map: data/mOTU.v1.padded

data/mOTU.v1.padded:
	mkdir -p data
	cd data && \
		wget http://vm-lux.embl.de/~kultima/share/mOTU/mOTU.v1.padded.tar.gz && \
		tar xf mOTU.v1.padded.tar.gz && \
		rm mOTU.v1.padded.tar.gz


.PHONY: all data
