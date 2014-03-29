all: nglessconf ngless nglesstest
.PHONY: clean ngless nglesstest

BWA = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

HTML = Html
HTML_LIBS_DIR = Html/htmllibs
HTML_FONTS_DIR = Html/fonts

GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures -threaded

CABAL_USER := ~/.cabal/bin/cabal

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

reqdeps:
	
install: nglessconf
	cabal configure
	cabal install

nglessconf: bwaconf confhtmllibs

ngless:
	cabal install

nglesstest:
	cabal install

clean:
	rm -rf $(BWA) $(HTML_LIBS_DIR) $(HTML_FONTS_DIR) $(64-MAC-PATH)*  dist .objs

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
	@if [ ! -d $(HTML)/fonts ]; then \
		mkdir $(HTML)/fonts;\
	fi

confhtmllibdir: 
	@if [ ! -d $(HTML_LIBS_DIR) ]; then \
		mkdir $(HTML_LIBS_DIR); \
	fi

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
	mkdir $(64-MAC-PATH)
	make ngless
	cp NGLess/ngless $(64-MAC-PATH)
	cp -r $(BWA) $(64-MAC-PATH)
	cp -r $(HTML) $(64-MAC-PATH)
	tar -zcvf $(64-MAC-PATH).tar.gz $(64-MAC-PATH)
	rm -rf $(64-MAC-PATH)

#########
