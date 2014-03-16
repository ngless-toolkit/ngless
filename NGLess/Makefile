all: setup ngless nglesstest
.PHONY: all clean
GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures -threaded

# Bwa name and location
BWA = ../bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

SOURCES :=  \
	Interpret.hs \
	Language.hs \
	Parse.hs \
	Tokens.hs \
	FastQFileData.hs \
	Validation.hs \
	PrintFastqBasicStats.hs \
	ProcessFastQ.hs \
	PerBaseQualityScores.hs \
	FPreProcess.hs \
	Unique.hs


ngless: Main.hs $(SOURCES)
	ghc $(GHCOPTS) --make -O2 $^ -o $@

nglesstest: Tests.hs $(SOURCES)
	ghc $(GHCOPTS) --make -O $^ -o $@

tests: nglesstest
	./nglesstest

clean:
	rm -rf .objs
	rm -f ngless nglesstest

setup:
	@echo Configuring BWA...
	@if [ ! -d $(BWA) ]; then \
		wget $(BWA_URL);\
		tar xvfj $(BWA_DIR) ;\
		rm $(BWA_DIR);\
		cd bwa-*;\
		make;\
		cd ..;\
		mv bwa-* ../ ;\
	fi

install:
	cabal install cmdargs
	cabal install happy
	cabal install test-framework-th
	cabal install test-framework-quickcheck2
	cabal install test-framework-hunit
	cabal install zlib
	cabal install parsec
	cabal install hashable
	cabal install aeson