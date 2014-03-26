all: install
.PHONY: clean ngless nglesstest

BWA = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

HTML = NGLess/Html
HTML_DIR = NGLess/Html/htmllibs

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
URLS += https://netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.woff
URLS += https://netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.ttf
GIT-LOGO += https://github-media-downloads.s3.amazonaws.com/Octocats.zip
#

install: bwaconf confhtmllibs
	cd NGLess && $(MAKE)

confhtmllibs: confhtmllibdir githublogo
	@echo configuring html libraries... 
	@$(foreach url,$(URLS), wget -nc -O $(HTML_DIR)/$(notdir $(url)) $(url) ; echo $(url) configured;)
	mkdir $(HTML)/fonts; mv $(HTML_DIR)/glyphicons-halflings-* $(HTML)/fonts/;

bwaconf: 
	@echo Configuring BWA...
	@if [ ! -d $(BWA) ]; then \
		wget $(BWA_URL);\
		tar xvfj $(BWA_DIR) ;\
		rm $(BWA_DIR);\
		cd bwa-*;\
		$(MAKE);\
	fi

ngless:
	cd NGLess && $(MAKE) ngless

nglesstest:
	cd NGLess && $(MAKE) nglesstest

tests:
	cd NGLess && $(MAKE) tests

clean:
	rm -rf $(BWA)
	cd NGLess && $(MAKE) clean


##### auxiliary functions to setup required files
confhtmllibdir: 
	@if [ ! -d $(HTML_DIR) ]; then \
		mkdir $(HTML_DIR); \
	fi

githublogo:
	wget -nc -O $(HTML_DIR)/$(notdir $(GIT-LOGO)) $(GIT-LOGO);
	unzip $(HTML_DIR)/$(notdir $(GIT-LOGO)) -d $(HTML_DIR);
	cp $(HTML_DIR)/Octocat/Octocat.png $(HTML_DIR)/Octocat.png ;  
	rm -rf $(HTML_DIR)/__MACOSX $(HTML_DIR)/Octocat $(HTML_DIR)/Octocats.zip ; 
	echo $(GIT-LOGO) configured;
#########
