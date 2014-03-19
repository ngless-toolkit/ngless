# Bwa name and location
BWA = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

all: install

install: bwaconf 
	cabal install cmdargs
	cabal install happy
	cabal install test-framework-th
	cabal install test-framework-quickcheck2
	cabal install test-framework-hunit
	cabal install zlib
	cabal install parsec
	cabal install hashable
	cabal install aeson
	cd NGLess && $(MAKE)

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
