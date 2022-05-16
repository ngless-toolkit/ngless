VERSION := $(shell ./build-scripts/read-version.sh)
progname := ngless
distdir := ngless-${VERSION}

prefix=/usr/local
deps=$(prefix)/share/$(progname)
exec=$(prefix)/bin

all: ngless

ngless:
	stack build $(STACKOPTS)

modules:
	cd Modules && $(MAKE)

static:
	stack build $(STACKOPTS) --ghc-options='-fPIC' --force-dirty --flag NGLess:static

fast:
	stack build --fast --work-dir .stack-work-fast $(STACKOPTS)

check:
	stack test --work-dir .stack-work-check $(STACKOPTS)

fastcheck:
	stack test --fast --work-dir .stack-work-fastcheck $(STACKOPTS)

dist: ngless-${VERSION}.tar.gz

# Synonym
tests: check

all_tests: check
	PATH=$(stack path --bin-path):${PATH} ./run-tests.sh

bench:
	stack bench --work-dir .stack-work-bench $(STACKOPTS)

profile:
	stack build $(STACKOPTS) --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

install: ngless $(MEGAHIT_BINS)
	mkdir -p $(exec)
	mkdir -p $(deps)/bin
	stack --local-bin-path $(exec) install $(STACKOPTS)

clean:
	stack clean $(STACKOPTS)

distclean: clean

ngless-${VERSION}.tar.gz: ngless
	mkdir -p $(distdir)/share $(distdir)/bin
	stack build $(STACKOPTS)
	cp dist/build/$(progname)/$(progname) $(distdir)/bin
	rm -rf $(distdir)

.PHONY: all build clean check tests all_tests distclean dist static fast fastcheck modules
