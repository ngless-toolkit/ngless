VERSION=1.0

progname=ngless

prefix=/usr/local
deps=$(prefix)/share/$(progname)
exec=$(prefix)/bin

SOURCES=NGLess/*

all: compile nglessconf

BWA = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

HTML = Html
HTML_LIBS_DIR = $(HTML)/htmllibs
HTML_FONTS_DIR = $(HTML)/fonts

current_dir = $(shell pwd)

# Required html Librarys
URLS := http://code.jquery.com/jquery-latest.min.js 
URLS += https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.1/angular.min.js
URLS += http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css
URLS += http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css
URLS += http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js
URLS += http://cdnjs.cloudflare.com/ajax/libs/d3/3.1.6/d3.min.js
URLS += http://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.js
URLS += http://cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.css
URLS += http://code.angularjs.org/1.3.0-beta.1/angular-sanitize.js
URLS += http://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap-glyphicons.css
URLS_FONTS := https://netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.woff
URLS_FONTS += https://netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.ttf
GIT-LOGO += https://github-media-downloads.s3.amazonaws.com/Octocats.zip
#

install: 
	cabal install --prefix=$(prefix)
	mkdir -p $(deps);
	cp -r $(HTML) $(deps)
	cp -r $(BWA) $(deps)

compile: nglessconf
	cabal sandbox init
	cabal install --only-dependencies --force-reinstalls

nglessconf: bwaconf confhtmllibs

clean:
	rm -rf $(BWA) $(HTML_LIBS_DIR) $(HTML_FONTS_DIR) $(64-MAC-PATH)*  dist .objs $(deps) $(exec)/ngless*
	cabal sandbox delete

clean-local-rep:
	rm -rf $(HTML_LIBS_DIR) $(HTML_FONTS_DIR) $(BWA)

variables:
	@echo $(BWA)
	@echo $(prefix)
	@echo $(deps)
	@echo $(exec)
	@echo $(HTML_LIBS_DIR)
	@echo $(HTML_FONTS_DIR)

##### auxiliary functions to setup required files

bwaconf: 
	@echo Configuring BWA...
	@if [ ! -d $(BWA) ]; then \
		wget $(BWA_URL);\
		tar xvfj $(BWA_DIR) ;\
		rm $(BWA_DIR);\
		cd $(BWA);\
		$(MAKE);\
	fi

confhtmllibs: confhtmllibdir conffonts githublogo
	@echo configuring html libraries...
	@$(foreach url,$(URLS), wget -nc -O $(HTML_LIBS_DIR)/$(notdir $(url)) $(url) ; echo $(url) configured;)
	@$(foreach url,$(URLS_FONTS), wget -nc -O $(HTML_FONTS_DIR)/$(notdir $(url)) $(url) ; echo $(url) configured;)

conffonts:
	mkdir -p $(HTML_FONTS_DIR);

confhtmllibdir:
	mkdir -p $(HTML_LIBS_DIR);

githublogo:
	@if [ ! -f $(HTML_LIBS_DIR)/Octocat.png ]; then \
		wget -nc -O $(HTML_LIBS_DIR)/$(notdir $(GIT-LOGO)) $(GIT-LOGO); \
		unzip $(HTML_LIBS_DIR)/$(notdir $(GIT-LOGO)) -d $(HTML_LIBS_DIR); \
		cp $(HTML_LIBS_DIR)/Octocat/Octocat.png $(HTML_LIBS_DIR)/Octocat.png; \
		rm -rf $(HTML_LIBS_DIR)/__MACOSX $(HTML_LIBS_DIR)/Octocat $(HTML_LIBS_DIR)/Octocats.zip; \
		echo $(GIT-LOGO) configured; \
	fi

######

#Generate self-contained executables
64-MAC-PATH := 64-Mac

64x-macos: nglessconf
	mkdir -p $(64-MAC-PATH)/share $(64-MAC-PATH)/bin
	cabal build
	cp dist/dist*/build/$(progname)/$(progname) $(64-MAC-PATH)/bin
	cp -r $(BWA) $(64-MAC-PATH)/share
	cp -r $(HTML) $(64-MAC-PATH)/share
	tar -zcvf $(64-MAC-PATH).tar.gz $(64-MAC-PATH)
	rm -rf $(64-MAC-PATH)

#########
