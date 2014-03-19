# Bwa name and location
BWA = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

all: install

install: bwaconf 
	cd NGLess && $(MAKE)
	cd NGLess && cabal install

bwaconf: 
	@echo Configuring BWA...
	@if [ ! -d $(BWA) ]; then \
		wget $(BWA_URL);\
		tar xvfj $(BWA_DIR) ;\
		rm $(BWA_DIR);\
		cd bwa-*;\
		$(MAKE);\
	fi


clean:
	rm -rf $(BWA)
	cd NGLess && $(MAKE) clean
