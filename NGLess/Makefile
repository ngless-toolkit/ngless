all: reqdeps confhtmllibs ngless nglesstest
.PHONY: all clean
GHCOPTS := -odir .objs -hidir .objs -Wall -fwarn-tabs -fno-warn-missing-signatures -threaded

# Required Librarys
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

HTML = Html
HTML_DIR = Html/htmllibs

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

confhtmllibdir: 
	@if [ ! -d $(HTML_DIR) ]; then \
		mkdir $(HTML_DIR); \
	fi

confhtmllibs: confhtmllibdir githublogo
	@echo configuring html libraries... 
	@$(foreach url,$(URLS), wget -nc -O $(HTML_DIR)/$(notdir $(url)) $(url) ; echo $(url) configured;)
	mkdir $(HTML)/fonts; mv $(HTML_DIR)/glyphicons-halflings-* $(HTML)/fonts/;

githublogo:
	wget -nc -O $(HTML_DIR)/$(notdir $(GIT-LOGO)) $(GIT-LOGO); 
	unzip $(HTML_DIR)/$(notdir $(GIT-LOGO)) -d $(HTML_DIR);
	cp $(HTML_DIR)/Octocat/Octocat.png $(HTML_DIR)/Octocat.png ;  
	rm -rf $(HTML_DIR)/__MACOSX $(HTML_DIR)/Octocat $(HTML_DIR)/Octocats.zip ; 
	echo $(GIT-LOGO) configured;


createsymlink: 
	rm /usr/local/bin/ngless        
	ln -s $(shell pwd)/ngless /usr/local/bin

clean:
	rm -rf .objs deps Html/htmllibs Html/fonts dist
	rm -f ngless nglesstest
