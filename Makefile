VERSION=1.0

progname=ngless

prefix=/usr/local
deps=$(prefix)/share/$(progname)
exec=$(prefix)/bin

SOURCES=NGLess/*

all: compile

BWA = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

SAM = samtools-0.1.19
SAM_URL = http://sourceforge.net/projects/samtools/files/samtools/0.1.19/samtools-0.1.19.tar.bz2
SAM_DIR = samtools-0.1.19.tar.bz2

HTML = Html
HTML_LIBS_DIR = $(HTML)/htmllibs
HTML_FONTS_DIR = $(HTML)/fonts

current_dir = $(shell pwd)

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

# Required fonts
FONTFILES := glyphicons-halflings-regular.woff
FONTFILES += glyphicons-halflings-regular.ttf

#URLS
jquery-latest.min.js = code.jquery.com/jquery-latest.min.js 
d3.min.js = cdnjs.cloudflare.com/ajax/libs/d3/3.1.6/d3.min.js
nv.d3.js = cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.js
nv.d3.css = cdnjs.cloudflare.com/ajax/libs/nvd3/1.1.14-beta/nv.d3.css
angular-sanitize.js = code.angularjs.org/1.3.0-beta.1/angular-sanitize.js
bootstrap.min.js = netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js
bootstrap.min.css = netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css
angular.min.js = ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.1/angular.min.js
bootstrap-glyphicons.css += netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap-glyphicons.css
bootstrap-theme.min.css = netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css
glyphicons-halflings-regular.woff = netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.woff
glyphicons-halflings-regular.ttf = netdna.bootstrapcdn.com/bootstrap/3.0.0/fonts/glyphicons-halflings-regular.ttf

GIT-LOGO += https://github-media-downloads.s3.amazonaws.com/Octocats.zip

reqhtmllibs = $(addprefix $(HTML_LIBS_DIR)/, $(HTMLFILES))
reqfonts = $(addprefix $(HTML_FONTS_DIR)/, $(FONTFILES))
reqLogo = $(HTML_LIBS_DIR)/Octocat.png
#

install: install-dir install-html install-bwa install-sam
	cp dist/build/nglesstest/nglesstest $(exec)/nglesstest
	cp dist/build/ngless/ngless $(exec)/ngless

install-html:
	cp -r $(HTML) $(deps)

install-bwa:
	cp -r $(BWA) $(deps)

install-sam:
	cp -r $(SAM) $(deps)

install-dir:
	mkdir -p $(exec)
	mkdir -p $(deps);

compile: nglessconf
	cabal sandbox init
	cabal install --only-dependencies --force-reinstalls
	cabal build

nglessconf: confhtmllibdir conffonts  $(SAM) $(BWA) $(reqhtmllibs) $(reqfonts) $(reqlogo)

clean:
	rm -rf $(BWA) $(SAM) $(HTML_LIBS_DIR) $(HTML_FONTS_DIR) $(64-MAC-PATH)*  dist .objs $(deps)
	cabal sandbox delete

variables:
	@echo $(BWA)
	@echo $(prefix)
	@echo $(deps)
	@echo $(exec)
	@echo $(HTML_LIBS_DIR)
	@echo $(HTML_FONTS_DIR)


uninstall:
	rm -rf $(deps) $(exec)/ngless* $(HOME)/.ngless

#####  Setup required files

$(BWA):
	@echo Configuring BWA...
	wget $(BWA_URL);
	tar xvfj $(BWA_DIR) ;
	rm $(BWA_DIR);
	cd $(BWA);
	$(MAKE);

$(SAM): 
	@echo Configuring SAM...
	wget $(SAM_URL);
	tar xvfj $(SAM_DIR) ;
	rm $(SAM_DIR);
	cd $(SAM);
	$(MAKE);
	@echo SAM completed...


$(HTML_LIBS_DIR)/%.js:
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))


$(HTML_LIBS_DIR)/%.css:
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))


$(HTML_FONTS_DIR)/%.woff:
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))

$(HTML_FONTS_DIR)/%.ttf:
	echo $(notdir $@)
	wget -O $@ $($(notdir $@))

confhtmllibs: 
	@echo configuring html libraries...
	@$(foreach url,$(URLS_FONTS), wget -nc -O $(HTML_FONTS_DIR)/$(notdir $(url)) $(url) ; echo $(url) configured;)

conffonts:
	mkdir -p $(HTML_FONTS_DIR);

confhtmllibdir:
	mkdir -p $(HTML_LIBS_DIR);

$(HTML_LIBS_DIR)/Octocat.png:
	wget -nc -O $(HTML_LIBS_DIR)/$(notdir $(GIT-LOGO)) $(GIT-LOGO);
	unzip $(HTML_LIBS_DIR)/$(notdir $(GIT-LOGO)) -d $(HTML_LIBS_DIR);
	cp $(HTML_LIBS_DIR)/Octocat/Octocat.png $(HTML_LIBS_DIR)/Octocat.png;
	rm -rf $(HTML_LIBS_DIR)/__MACOSX $(HTML_LIBS_DIR)/Octocat $(HTML_LIBS_DIR)/Octocats.zip;
	echo $(GIT-LOGO) configured;

######


#Generate self-contained executables
64-MAC-PATH := 64-Mac

64x-macos: nglessconf
	mkdir -p $(64-MAC-PATH)/share $(64-MAC-PATH)/bin
	cabal build
	cp dist/build/$(progname)/$(progname) $(64-MAC-PATH)/bin
	cp -r $(BWA) $(64-MAC-PATH)/share
	cp -r $(SAM) $(64-MAC-PATH)/share
	cp -r $(HTML) $(64-MAC-PATH)/share
	tar -zcvf $(64-MAC-PATH).tar.gz $(64-MAC-PATH)
	rm -rf $(64-MAC-PATH)

#########
