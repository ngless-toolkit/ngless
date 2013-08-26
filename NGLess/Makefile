all: ngless nglesstest
.PHONY: all clean
GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures

SOURCES :=  \
	Interpret.hs \
	Language.hs \
	Parse.hs \
	Tokens.hs \
	NumberOfChars.hs \
	Validation.hs \
	PrintFastqBasicStats.hs

ngless: Main.hs $(SOURCES)
	ghc $(GHCOPTS) --make -O2 $^ -o $@

nglesstest: Tests.hs $(SOURCES)
	ghc $(GHCOPTS) --make -O $^ -o $@

tests: nglesstest
	./nglesstest

clean:
	rm -rf .objs
	rm -f ngless nglesstest
