all: setup ngless nglesstest
.PHONY: all clean
GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures -threaded

# Bwa name and location
BWA = ../bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

# Required Librarys
URLS := http://code.jquery.com/jquery-latest.min.js
URLS += https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.1/angular.min.js
URLS += http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css
URLS += http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css
URLS += http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js
URLS += http://cdnjs.cloudflare.com/ajax/libs/d3/3.1.6/d3.min.js
URLS += http://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.js
URLS += http://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.css
#

HTML_DIR = ../htmllibs

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

XPTO := 

ngless: Main.hs $(SOURCES)
	ghc $(GHCOPTS) --make -O2 $^ -o $@

nglesstest: Tests.hs $(SOURCES)
	ghc $(GHCOPTS) --make -O $^ -o $@

tests: nglesstest
	./nglesstest

clean:
	rm -rf .objs
	rm -f ngless nglesstest

setup: htmllibs
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

htmllibsdir: 
	@if [ ! -d $(HTML_DIR) ]; then \
		mkdir $(HTML_DIR); \
	fi

htmllibs: htmllibsdir
	@echo configuring html libraries... 
	@$(foreach url,$(URLS), wget -nc -O $(HTML_DIR)/$(notdir $(url)) $(url) ; echo $(url) configured;)

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