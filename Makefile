# Bwa name and location
.PHONY: clean ngless

BWA = bwa-0.7.7
BWA_URL = http://sourceforge.net/projects/bio-bwa/files/bwa-0.7.7.tar.bz2
BWA_DIR = bwa-0.7.7.tar.bz2

all: install

install: bwaconf 
	cd NGLess && $(MAKE)

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

clean:
	rm -rf $(BWA)
	cd NGLess && $(MAKE) clean
