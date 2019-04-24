VERSION := $(shell ./build-scripts/read-version.sh)
progname := ngless
distdir := ngless-${VERSION}

SHELL=bash
prefix=/usr/local
deps=$(prefix)/share/$(progname)
exec=$(prefix)/bin

WGET := wget

# We can't depend on [BWA/SAM/MEGAHIT]_DIR as a build target.
# These change every time a file inside is created/modified
# As a workaround we target a file included in the tarball
BWA_VERSION = 0.7.17
BWA_DIR = bwa-$(BWA_VERSION)
BWA_DIR_TARGET = $(BWA_DIR)/Makefile
BWA_URL = https://github.com/lh3/bwa/releases/download/v$(BWA_VERSION)/bwa-$(BWA_VERSION).tar.bz2
BWA_TAR = bwa-$(BWA_VERSION).tar.bz2
BWA_SHA1 = d5640b5083a8d38878c385c23261c659ab3229ef
BWA_TARGET = ngless-bwa
BWA_TARGET_VERSIONED = ngless-${VERSION}-bwa

SAM_VERSION = 1.6
SAM_DIR = samtools-$(SAM_VERSION)
SAM_DIR_TARGET = $(SAM_DIR)/configure
SAM_URL = https://github.com/samtools/samtools/releases/download/$(SAM_VERSION)/samtools-$(SAM_VERSION).tar.bz2
SAM_TAR = samtools-$(SAM_VERSION).tar.bz2
SAM_SHA1 = c72c059b0d6525d1b12acbe25cc1f69157d4da7a
SAM_TARGET = ngless-samtools
SAM_TARGET_VERSIONED = ngless-${VERSION}-samtools

PRODIGAL_VERSION = 2.6.3
PRODIGAL_DIR = Prodigal-$(PRODIGAL_VERSION)
PRODIGAL_DIR_TARGET = $(PRODIGAL_DIR)/Makefile
PRODIGAL_URL = https://github.com/hyattpd/Prodigal/archive/v$(PRODIGAL_VERSION).tar.gz
PRODIGAL_TAR = v$(PRODIGAL_VERSION).tar.gz
PRODIGAL_SHA1 = 1259e999193cd0c095935baebfb8bcb0233e850f
PRODIGAL_TARGET = ngless-prodigal
PRODIGAL_TARGET_VERSIONED = ngless-${VERSION}-prodigal

MEGAHIT_VERSION = 1.1.4
MEGAHIT_DIR = megahit-$(MEGAHIT_VERSION)
# we can't target Makefile here cause we patch it after unpacking
MEGAHIT_DIR_TARGET = $(MEGAHIT_DIR)/LICENSE
MEGAHIT_URL = https://github.com/voutcn/megahit/archive/v$(MEGAHIT_VERSION).tar.gz
MEGAHIT_TAR = v$(MEGAHIT_VERSION).tar.gz
MEGAHIT_SHA1 = 2707cb46f7dde579065b65374a8053ba68d54dff
MEGAHIT_TARGET = megahit

MINIMAP2_VERSION = 2.16
MINIMAP2_DIR = minimap2-$(MINIMAP2_VERSION)
MINIMAP2_URL = https://github.com/lh3/minimap2/releases/download/v$(MINIMAP2_VERSION)/minimap2-$(MINIMAP2_VERSION).tar.bz2
MINIMAP2_TAR = minimap2-$(MINIMAP2_VERSION).tar.bz2
MINIMAP2_SHA1 = 41a5a31394b41b1c2ba03ce29290b0ecda0a1e8a
MINIMAP2_TARGET = ngless-minimap2
MINIMAP2_TARGET_VERSIONED = ngless-${VERSION}-minimap2

NGLESS_VERSIONS_TARGET = NGLess/Dependencies/Versions.hs
NGLESS_EMBEDDED_BINARIES := \
		NGLess/Dependencies/samtools_data.c \
		NGLess/Dependencies/prodigal_data.c \
		NGLess/Dependencies/bwa_data.c \
		NGLess/Dependencies/megahit_data.c \
		NGLess/Dependencies/minimap2_data.c \
		$(NGLESS_VERSIONS_TARGET)
NGLESS_EMBEDDED_TARGET = NGLess/Dependencies/embedded.c

MEGAHIT_BINS := $(MEGAHIT_DIR)/megahit_asm_core $(MEGAHIT_DIR)/megahit_sdbg_build $(MEGAHIT_DIR)/megahit_toolkit $(MEGAHIT_DIR)/megahit
NGLESS_EXT_BINS_VERSIONED = $(BWA_DIR)/$(BWA_TARGET_VERSIONED) \
							$(SAM_DIR)/$(SAM_TARGET_VERSIONED) \
							$(PRODIGAL_DIR)/$(PRODIGAL_TARGET_VERSIONED) \
							$(MINIMAP2_DIR)/$(MINIMAP2_TARGET_VERSIONED)

HTML = Html
HTML_LIBS_DIR = $(HTML)/htmllibs
HTML_FONTS_DIR = $(HTML)/fonts

# Required html Librarys
HTMLFILES := jquery-latest.min.js
HTMLFILES += angular.min.js
HTMLFILES += bootstrap.min.css
HTMLFILES += bootstrap-theme.min.css
HTMLFILES += bootstrap.min.js
HTMLFILES += d3.min.js
HTMLFILES += nv.d3.js
HTMLFILES += nv.d3.css
HTMLFILES += angular-sanitize.js
HTMLFILES += bootstrap-glyphicons.css
HTMLFILES += angular-animate.min.js

# Required fonts
FONTFILES := glyphicons-halflings-regular.woff
FONTFILES += glyphicons-halflings-regular.ttf

#URLS
jquery-latest.min.js = https://code.jquery.com/jquery-latest.min.js
d3.min.js = https://cdnjs.cloudflare.com/ajax/libs/d3/3.1.6/d3.min.js
nv.d3.js = https://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.js
nv.d3.css = https://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.css
angular-sanitize.js = https://code.angularjs.org/1.3.0-beta.1/angular-sanitize.js
bootstrap.min.js = https://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js
bootstrap.min.css = https://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css
angular.min.js = https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.1/angular.min.js
bootstrap-glyphicons.css += https://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap-glyphicons.css
bootstrap-theme.min.css = https://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css
glyphicons-halflings-regular.woff = https://netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.woff
glyphicons-halflings-regular.ttf = https://netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.ttf
angular-animate.min.js = https://ajax.googleapis.com/ajax/libs/angularjs/1.2.16/angular-animate.min.js

reqhtmllibs = $(addprefix $(HTML_LIBS_DIR)/, $(HTMLFILES))
reqfonts = $(addprefix $(HTML_FONTS_DIR)/, $(FONTFILES))

all: ngless

ngless: $(NGLESS_VERSIONS_TARGET)
	stack build $(STACKOPTS)

modules:
	cd Modules && $(MAKE)

static: $(NGLESS_EMBEDDED_TARGET) external-deps
	stack build $(STACKOPTS) --ghc-options='-fPIC' --force-dirty --flag NGLess:embed

fast: $(NGLESS_VERSIONS_TARGET)
	stack build --fast $(STACKOPTS)

dist: ngless-${VERSION}.tar.gz

check: $(NGLESS_VERSIONS_TARGET)
	stack test --work-dir .stack-work-test $(STACKOPTS)

fastcheck: $(NGLESS_VERSIONS_TARGET)
	stack test --fast --work-dir .stack-work-test $(STACKOPTS)

# Synonym
tests: check

all_tests: check
	PATH=$(stack path --bin-path):${PATH} ./run-tests.sh

bench:
	stack bench $(STACKOPTS)

profile: $(NGLESS_VERSIONS_TARGET)
	stack build $(STACKOPTS) --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

install: ngless external-deps $(NGLESS_EXT_BINS_VERSIONED) $(MEGAHIT_BINS)
	mkdir -p $(exec)
	mkdir -p $(deps)/bin
	stack --local-bin-path $(exec) install $(STACKOPTS)
	cp -prf $(HTML) $(deps)
	cp -prf $(NGLESS_EXT_BINS_VERSIONED) $(deps)/bin
	mkdir -p $(deps)/bin/ngless-${VERSION}-megahit
	cp -prf $(MEGAHIT_BINS) $(deps)/bin/ngless-${VERSION}-megahit

external-deps: $(NGLESS_EXT_BINS_VERSIONED) $(reqhtmllibs) $(reqfonts)

clean:
	rm -f $(NGLESS_EMBEDDED_BINARIES)
	stack clean $(STACKOPTS)

distclean: clean
	rm -rf $(HTML_FONTS_DIR) $(HTML_LIBS_DIR)
	rm -rf $(BWA_DIR)
	rm -rf $(SAM_DIR)
	rm -rf $(PRODIGAL_DIR)
	rm -rf $(MEGAHIT_DIR)

$(NGLESS_EMBEDDED_TARGET): $(NGLESS_EMBEDDED_BINARIES)
	touch $(NGLESS_EMBEDDED_TARGET)

$(BWA_DIR_TARGET):
	$(WGET) $(BWA_URL) -O $(BWA_TAR)
	sha1sum -c <(echo "$(BWA_SHA1)  $(BWA_TAR)")
	tar xvf $(BWA_TAR)
	rm $(BWA_TAR)

$(BWA_DIR)/$(BWA_TARGET): $(BWA_DIR_TARGET)
	cd $(BWA_DIR) && $(MAKE) && mv bwa $(BWA_TARGET)

$(BWA_DIR)/$(BWA_TARGET)-static: $(BWA_DIR_TARGET)
	cd $(BWA_DIR) && $(MAKE) CFLAGS="-O2 -static"  LIBS="-lbwa -lm -lz -lrt -lpthread" && cp -p bwa $(BWA_TARGET)-static

$(BWA_DIR)/$(BWA_TARGET_VERSIONED): $(BWA_DIR)/$(BWA_TARGET)
	cp $< $@

$(SAM_DIR_TARGET):
	$(WGET) $(SAM_URL) -O $(SAM_TAR)
	sha1sum -c <(echo "$(SAM_SHA1)  $(SAM_TAR)")
	tar xvf $(SAM_TAR)
	rm $(SAM_TAR)

$(SAM_DIR)/$(SAM_TARGET)-static: $(SAM_DIR_TARGET)
	cd $(SAM_DIR) && ./configure --without-curses && $(MAKE) LDFLAGS="-static" DFLAGS="-DNCURSES_STATIC" && cp -p samtools $(SAM_TARGET)-static

$(SAM_DIR)/$(SAM_TARGET): $(SAM_DIR_TARGET)
	cd $(SAM_DIR) && ./configure --without-curses && $(MAKE) && cp -p samtools $(SAM_TARGET)

$(SAM_DIR)/$(SAM_TARGET_VERSIONED): $(SAM_DIR)/$(SAM_TARGET)
	cp $< $@

$(PRODIGAL_DIR_TARGET):
	$(WGET) $(PRODIGAL_URL) -O $(PRODIGAL_TAR)
	sha1sum -c <(echo "$(PRODIGAL_SHA1)  $(PRODIGAL_TAR)")
	tar xvf $(PRODIGAL_TAR)
	rm $(PRODIGAL_TAR)
	cd $(PRODIGAL_DIR) && patch -p1 <../build-scripts/0001-Fix-undefined-behavior-causing-behavior.patch

$(PRODIGAL_DIR)/$(PRODIGAL_TARGET)-static: $(PRODIGAL_DIR_TARGET)
	cd $(PRODIGAL_DIR) && $(MAKE) CFLAGS="-O2 -static" && cp -p prodigal $(PRODIGAL_TARGET)-static

$(PRODIGAL_DIR)/$(PRODIGAL_TARGET): $(PRODIGAL_DIR_TARGET)
	cd $(PRODIGAL_DIR) && $(MAKE) && cp -p prodigal $(PRODIGAL_TARGET)

$(PRODIGAL_DIR)/$(PRODIGAL_TARGET_VERSIONED): $(PRODIGAL_DIR)/$(PRODIGAL_TARGET)
	cp $< $@

$(MEGAHIT_DIR_TARGET):
	$(WGET) $(MEGAHIT_URL) -O $(MEGAHIT_TAR)
	sha1sum -c <(echo "$(MEGAHIT_SHA1)  $(MEGAHIT_TAR)")
	tar xvf $(MEGAHIT_TAR)
	rm $(MEGAHIT_TAR)
	cd $(MEGAHIT_DIR) && patch -p1 <../build-scripts/megahit-1.1.1.patch

$(MEGAHIT_DIR)/static-build: $(MEGAHIT_DIR_TARGET)
	cd $(MEGAHIT_DIR) && $(MAKE) clean && $(MAKE) CXXFLAGS=-static
	touch $@

$(MEGAHIT_DIR)/$(MEGAHIT_TARGET): $(MEGAHIT_DIR_TARGET)
	cd $(MEGAHIT_DIR) && $(MAKE) clean && $(MAKE)
	rm -f $(MEGAHIT_DIR)/static-build

megahit-static: $(MEGAHIT_DIR)/static-build

megahit: $(MEGAHIT_DIR)/$(MEGAHIT_TARGET)
$(MEGAHIT_DIR)/megahit_asm_core: megahit
$(MEGAHIT_DIR)/megahit_sdbg_build: megahit
$(MEGAHIT_DIR)/megahit_toolkit: megahit

$(MEGAHIT_DIR)/$(MEGAHIT_TARGET)-packaged: $(MEGAHIT_DIR)/static-build
	cd $(MEGAHIT_DIR) && strip megahit_asm_core
	cd $(MEGAHIT_DIR) && strip megahit_sdbg_build
	cd $(MEGAHIT_DIR) && strip megahit_toolkit
	mkdir -p $@ && cp -pr $(MEGAHIT_BINS) $@

$(MEGAHIT_DIR)/$(MEGAHIT_TARGET)-packaged.tar.gz: $(MEGAHIT_DIR)/$(MEGAHIT_TARGET)-packaged
	tar --create --file $@ --gzip $<

$(MINIMAP2_DIR)/README.md:
	$(WGET) $(MINIMAP2_URL) -O $(MINIMAP2_TAR)
	sha1sum -c <(echo "$(MINIMAP2_SHA1)  $(MINIMAP2_TAR)")
	tar xvf $(MINIMAP2_TAR)
	rm $(MINIMAP2_TAR)

$(MINIMAP2_DIR)/$(MINIMAP2_TARGET)-static: $(MINIMAP2_DIR)/README.md
	rm -f $@
	cd $(MINIMAP2_DIR) && $(MAKE) CFLAGS="-O2 -DHAVE_GETOPT -static" && mv minimap2 ngless-minimap2-static

$(MINIMAP2_DIR)/$(MINIMAP2_TARGET): $(MINIMAP2_DIR)/README.md
	cd $(MINIMAP2_DIR) && $(MAKE) && mv minimap2 ngless-minimap2

$(MINIMAP2_DIR)/$(MINIMAP2_TARGET_VERSIONED): $(MINIMAP2_DIR)/$(MINIMAP2_TARGET)
	cp -pr $< $@

# The following empty newline, VERSION_BODY and NGLESS_VERSIONS_TARGET
# rule below are required to make versions available to NGLess during execution
#
# This approach was taken from https://stackoverflow.com/a/5887751
define NEWLINE


endef

define VERSION_BODY
module Dependencies.Versions
    ( bwaVersion
    , samtoolsVersion
    , prodigalVersion
    , megahitVersion
    , minimap2Version
    ) where

bwaVersion :: String
bwaVersion = "$(BWA_VERSION)"
samtoolsVersion :: String
samtoolsVersion = "$(SAM_VERSION)"
prodigalVersion :: String
prodigalVersion = "$(PRODIGAL_VERSION)"
megahitVersion :: String
megahitVersion = "$(MEGAHIT_VERSION)"
minimap2Version :: String
minimap2Version = "$(MINIMAP2_VERSION)"
endef

$(NGLESS_VERSIONS_TARGET): Makefile
	@echo -e '$(subst $(NEWLINE),\n,${VERSION_BODY})' > $@

NGLess/Dependencies/samtools_data.c: $(SAM_DIR)/$(SAM_TARGET)-static
	strip $<
	ln -s $< $(<F)
	xxd -i $(<F) $@
	rm -f $(<F)

NGLess/Dependencies/prodigal_data.c: $(PRODIGAL_DIR)/$(PRODIGAL_TARGET)-static
	strip $<
	ln -s $< $(<F)
	xxd -i $(<F) $@
	rm -f $(<F)

NGLess/Dependencies/bwa_data.c: $(BWA_DIR)/$(BWA_TARGET)-static
	strip $<
	ln -s $< $(<F)
	xxd -i $(<F) $@
	rm -f $(<F)

NGLess/Dependencies/megahit_data.c: $(MEGAHIT_DIR)/$(MEGAHIT_TARGET)-packaged.tar.gz
	ln -s $< $(<F)
	xxd -i $(<F) $@
	rm -f $(<F)

NGLess/Dependencies/minimap2_data.c: $(MINIMAP2_DIR)/$(MINIMAP2_TARGET)-static
	strip $<
	ln -s $< $(<F)
	xxd -i $(<F) $@
	rm -f $(<F)

# We cannot depend on $(HTML_LIBS_DIR) as $(WGET) sets the mtime in the past
# and it would cause the download to happen at every make run
$(HTML_LIBS_DIR)/%.js:
	mkdir -p $(HTML_LIBS_DIR)
	echo $(notdir $@)
	$(WGET) -O $@ $($(notdir $@))


$(HTML_LIBS_DIR)/%.css:
	mkdir -p $(HTML_LIBS_DIR)
	echo $(notdir $@)
	$(WGET) -O $@ $($(notdir $@))


$(HTML_FONTS_DIR)/%.woff:
	mkdir -p $(HTML_FONTS_DIR)
	echo $(notdir $@)
	$(WGET) -O $@ $($(notdir $@))

$(HTML_FONTS_DIR)/%.ttf:
	mkdir -p $(HTML_FONTS_DIR)
	echo $(notdir $@)
	$(WGET) -O $@ $($(notdir $@))

ngless-${VERSION}.tar.gz: ngless
	mkdir -p $(distdir)/share $(distdir)/bin
	stack build $(STACKOPTS)
	cp dist/build/$(progname)/$(progname) $(distdir)/bin
	cp -r $(BWA_DIR) $(distdir)/share
	cp -r $(SAM_DIR) $(distdir)/share
	cp -r $(PRODIGAL_DIR) $(distdir)/share
	cp -r $(MEGAHIT_DIR) $(distdir)/share
	cp -r $(HTML) $(distdir)/share
	tar -zcvf $(distdir).tar.gz $(distdir)
	rm -rf $(distdir)

.PHONY: all build clean check tests all_tests distclean dist static fast fastcheck modules external-deps megahit
