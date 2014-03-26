all: reqdeps createsymlink nglesstest
.PHONY: all clean
GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures -threaded

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

reqdeps: 
	cabal install --force-reinstalls

createsymlink: ngless
	echo linking to ngless at /usr/local/bin
	@rm -f /usr/local/bin/ngless        
	ln -s $(shell pwd)/ngless /usr/local/bin
	echo linking to ngless completed

clean:
	rm -rf .objs deps Html/htmllibs Html/fonts dist
	rm -f ngless nglesstest
