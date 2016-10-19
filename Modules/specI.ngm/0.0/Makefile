RefMG.v1.padded.gtf: create-gtf-file.py data/
	python $< > $@

data/:
	mkdir -p data
	cd data && wget http://vm-lux.embl.de/~kultima/share/mOTU/RefMG.v1.padded.tar.gz && tar xzf RefMG.v1.padded.tar.gz

