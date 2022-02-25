VERSION := $(shell ./build-scripts/read-version.sh)
progname := ngless
distdir := ngless-${VERSION}

SHELL=bash
prefix=/usr/local
deps=$(prefix)/share/$(progname)
exec=$(prefix)/bin

HTML = Html


all: ngless

ngless: $(NGLESS_VERSIONS_TARGET)
	stack build $(STACKOPTS)

modules:
	cd Modules && $(MAKE)

static:
	stack build $(STACKOPTS) --ghc-options='-fPIC' --force-dirty

fast: $(NGLESS_VERSIONS_TARGET)
	stack build --fast --work-dir .stack-work-fast $(STACKOPTS)

check: $(NGLESS_VERSIONS_TARGET)
	stack test --work-dir .stack-work-check $(STACKOPTS)

fastcheck: $(NGLESS_VERSIONS_TARGET)
	stack test --fast --work-dir .stack-work-fastcheck $(STACKOPTS)

dist: ngless-${VERSION}.tar.gz

# Synonym
tests: check

all_tests: check
	PATH=$(stack path --bin-path):${PATH} ./run-tests.sh

bench:
	stack bench $(STACKOPTS)

profile: $(NGLESS_VERSIONS_TARGET)
	stack build $(STACKOPTS) --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

install: ngless $(NGLESS_EXT_BINS_VERSIONED)
	mkdir -p $(exec)
	mkdir -p $(deps)/bin
	stack --local-bin-path $(exec) install $(STACKOPTS)

clean:
	stack clean $(STACKOPTS)



.PHONY: all build clean check tests all_tests distclean dist static fast fastcheck modules megahit
