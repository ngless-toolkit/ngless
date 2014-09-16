VERSION := 0.0.0

progname := ngless
distdir := ngless-${VERSION}

prefix=/usr/local
deps=$(prefix)/share/$(progname)
exec=$(prefix)/bin


BWA_DIR = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_TAR = bwa-0.7.7.tar.bz2

SAM_DIR = samtools-1.0
SAM_URL = http://sourceforge.net/projects/samtools/files/samtools/1.0/samtools-1.0.tar.bz2
SAM_TAR = samtools-1.0.tar.bz2

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
HTMLFILES += ng-table.js
HTMLFILES += ng-table.css
HTMLFILES += angular-ui-router.min.js
HTMLFILES += angular-animate.min.js
HTMLFILES += ace.js
HTMLFILES += mode-python.js

# Required fonts
FONTFILES := glyphicons-halflings-regular.woff
FONTFILES += glyphicons-halflings-regular.ttf

#URLS
jquery-latest.min.js = code.jquery.com/jquery-latest.min.js
d3.min.js = cdnjs.cloudflare.com/ajax/libs/d3/3.1.6/d3.min.js
nv.d3.js = cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.js
nv.d3.css = cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.css
ng-table.js = bazalt-cms.com/assets/ng-table/0.3.0/ng-table.js
ng-table.css = bazalt-cms.com/assets/ng-table/0.3.0/ng-table.css
angular-sanitize.js = code.angularjs.org/1.3.0-beta.1/angular-sanitize.js
bootstrap.min.js = netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js
bootstrap.min.css = netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css
angular.min.js = ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.1/angular.min.js
bootstrap-glyphicons.css += netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap-glyphicons.css
bootstrap-theme.min.css = netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css
glyphicons-halflings-regular.woff = netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.woff
glyphicons-halflings-regular.ttf = netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.ttf
angular-animate.min.js = ajax.googleapis.com/ajax/libs/angularjs/1.2.16/angular-animate.min.js
angular-ui-router.min.js = cdnjs.cloudflare.com/ajax/libs/angular-ui-router/0.2.10/angular-ui-router.min.js
ace.js = cdnjs.cloudflare.com/ajax/libs/ace/1.1.3/ace.js
mode-python.js = cdnjs.cloudflare.com/ajax/libs/ace/1.1.3/mode-python.js

GIT_LOGO := github-media-downloads.s3.amazonaws.com/Octocats.zip

reqhtmllibs = $(addprefix $(HTML_LIBS_DIR)/, $(HTMLFILES))
reqfonts = $(addprefix $(HTML_FONTS_DIR)/, $(FONTFILES))
reqlogo = $(HTML_LIBS_DIR)/Octocat.png

all: nglessconf
	cabal build

dist: ngless-${VERSION}.tar.gz

testinputfiles=test_samples/htseq-res/htseq_cds_noStrand_union.txt

test_samples/htseq-res/htseq_cds_noStrand_union.txt:
	cd test_samples/ && gzip -dkf *.gz
	cd test_samples/htseq-res && ./generateHtseqFiles.sh


installtestdeps=dist/build/share/Html/nglessKeeper.html
dist/build/share/Html/nglessKeeper.html: nglessconf

check: ${testinputfiles} ${installtestdeps} simulateinstalldeps
	cabal test

simulateinstalldeps:
	mkdir -p dist/build/share/$(progname)
	cp -rf $(HTML) dist/build/share/$(progname)
	cp -rf $(BWA_DIR) dist/build/share/$(progname)
	cp -rf $(SAM_DIR) dist/build/share/$(progname)

install:
	mkdir -p $(exec)
	mkdir -p $(deps)
	cp -rf $(HTML) $(deps)
	cp -rf $(BWA_DIR) $(deps)
	cp -rf $(SAM_DIR) $(deps)
	cp -f dist/build/ngless/ngless $(exec)/ngless

nglessconf: cabal.sandbox.config $(SAM_DIR) $(BWA_DIR) $(reqhtmllibs) $(reqfonts) $(reqlogo)

cabal.sandbox.config: NGLess.cabal
	cabal sandbox init
	cabal install --only-dependencies --force-reinstalls

clean:
	rm -rf dist

distclean: clean
	if [ -d .cabal.sandobox ]; then cabal sandbox delete; fi
	rm -rf $(HTML_FONTS_DIR) $(HTML_LIBS_DIR)
	rm -rf $(BWA_DIR)
	rm -rf $(SAM_DIR)
	rm -f test_samples/htseq-res/*.txt

uninstall:
	rm -rf $(deps) $(exec)/ngless* $(HOME)/.ngless


#####  Setup required files
$(BWA_DIR):
	wget $(BWA_URL)
	tar xvfj $(BWA_TAR)
	rm $(BWA_TAR)

$(BWA_DIR)/ngless-bwa-static: $(BWA_DIR)
	cd $(BWA_DIR) && $(MAKE) CFLAGS="-static" && cp bwa ngless-bwa-static

$(SAM_DIR):
	wget $(SAM_URL)
	tar xvfj $(SAM_TAR)
	rm $(SAM_TAR)

$(SAM_DIR)/samtools: $(SAM_DIR)
	cd $(SAM_DIR) && $(MAKE) LDFLAGS="-static" DFLAGS="-DNCURSES_STATIC"


# We cannot depend on $(HTML_LIBS_DIR) as wget sets the mtime in the past
# and it would cause the download to happen at every make run
$(HTML_LIBS_DIR)/%.js:
	mkdir -p $(HTML_LIBS_DIR)
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))


$(HTML_LIBS_DIR)/%.css:
	mkdir -p $(HTML_LIBS_DIR)
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))


$(HTML_FONTS_DIR)/%.woff:
	mkdir -p $(HTML_FONTS_DIR)
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))

$(HTML_FONTS_DIR)/%.ttf:
	mkdir -p $(HTML_FONTS_DIR)
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))

$(HTML_LIBS_DIR)/Octocat.png:
	mkdir -p $(HTML_LIBS_DIR)
	wget -O $(HTML_LIBS_DIR)/$(notdir $(GIT_LOGO)) $(GIT_LOGO);
	unzip $(HTML_LIBS_DIR)/$(notdir $(GIT_LOGO)) -d $(HTML_LIBS_DIR);
	cp $(HTML_LIBS_DIR)/Octocat/Octocat.png $(HTML_LIBS_DIR)/Octocat.png;
	rm -rf $(HTML_LIBS_DIR)/__MACOSX $(HTML_LIBS_DIR)/Octocat $(HTML_LIBS_DIR)/Octocats.zip;
	echo $(GIT_LOGO) configured;



ngless-${VERSION}.tar.gz: nglessconf
	mkdir -p $(distdir)/share $(distdir)/bin
	cabal build
	cp dist/build/$(progname)/$(progname) $(distdir)/bin
	cp -r $(BWA_DIR) $(distdir)/share
	cp -r $(SAM_DIR) $(distdir)/share
	cp -r $(HTML) $(distdir)/share
	tar -zcvf $(distdir).tar.gz $(distdir)
	rm -rf $(distdir)

.PHONY: build clean check nglessconf distclean dist
