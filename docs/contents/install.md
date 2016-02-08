# Installation

**NOTE**: As of Feb 2016, ngless is available only as a pre-release to allow
for testing of early versions by fellow scientists. We do not recommend use in
production. Please [get in touch](mailto:coelho@embl.de) if you are interested
in using ngless in your projects.

## From source

[Stack](http://docs.haskellstack.org/en/stable/README.html) is the simplest way
to install the necessary requirements. You should also have `gcc` installed (or
another C-compiler).

The following sequence of commands should download and build the software

    git clone https://github.com/luispedro/ngless
    cd ngless
    make


The first time you run this, it will take a while as it will download all
dependencies. After this ngless is ready to use!


## Make targets

The following are targets in the Makefile.

make - compiles NGLess and haskell dependencies
clean - remove local generated files by compilation
check - run tests
bench - run benchmarks

## FAQ

### During 'make' an error was reported saying : 'curses.h: No such file or directory.'. How do I fix it?

You need to install the curses library which include routines for a
terminal-independent method of updating character screens with reasonable
optimization.  The fix depends on the Operating System you are currently in.

Ubuntu:
	
	sudo apt-get install libncurses5-dev libncursesw5-dev
		
Fedora / RHEL / CentOS Linux:
	
	yum install ncurses-devel ncurses
	
## During 'make' an error was reported saying 'Error: SSE2 instruction set not enabled'. How do I fix it?

This is a known problem when compiling the Burrows-Wheeler Aligner(BWA) tool
when under a **32 bits** operating system. To fix it, you have to change the
Makefile inside the directory **bwa-0.7.7/** in

	Line 3) CFLAGS= -g -Wall -O2 -msse -mmmx -msse2

	Line 6) DFLAGS= -DHAVE_PTHREAD
	
