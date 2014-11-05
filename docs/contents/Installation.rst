.. _Installation:

============
Installation
============

Automated
---------

From source
-----------

Requirements
~~~~~~~~~~~~

Before starting, make sure you have an Internet connection!

These are the required programs you must have installed

- GHC (version 7.6.3 or higher)
- Cabal (1.8.0.3 or higher)
- Git

Both GHC and Cabal can be installed via the haskell-platform package.

Update cabal
~~~~~~~~~~~~

Cabal by default comes with an older version installed. Start by running::

	$ cabal --version

If the version is equal or higher than 1.8.0.3 you are ready to install NGLess and no more steps are required!  Otherwise continue this installation process which will update your cabal version. Start by running the following commands::

	$ cabal update
	$ sudo cabal install cabal-install --prefix=/usr

These commands require **super user privilege** and install the correct version of cabal in '/usr' which is by default in **$PATH**. In case, you only have **user privileges** you can choose a **--prefix** anywhere else (that you have permissions) but **make sure that is in your $PATH environment**. 
After the installation, you should be ready to go. To be sure, run again::

	$ cabal --version

Check if the new version is higher than 1.8.0.3. 

If it is, you are ready to install NGLess.

If it isn't, one of the following problems might be occurring

	1) The path used as --prefix is not in your $PATH.

	2) An older version of cabal is installed in some directory which comes first than '/usr/local' in your $PATH variable.


Steps
~~~~~~~~~~

Start by download latest NGLess version from Github.::

    $ git clone https://github.com/luispedro/ngless
    $ cd ngless

Then download and configure all NGLess dependencies by running the following command.::

    $ make

This will take a while, so go ahead and make some tea! After the previous command is completed (without errors) you are ready to install ngless wherever you want it to be.
::

  $ sudo make install prefix=dir (default dir is /usr/local)

(Note: **sudo** is only required when installing on a directoy with **super user privileges**)

After this ngless is ready to use!


Options:
~~~~~~~~~~

The following are options to the Makefile.

clean - remove local generated files by compilation

uninstall - remove installed files. By default assumes installation in /usr/local, but prefix=dir can be passed

compile - compiles NGLess and haskell dependencies

nglessconf - downloads and configures all ngless dependencies

Binary
--------

A self-contained NGLess distribution is also available for:

	OS X Mavericks (x86\_64): **ngless-0.0.0-Darwin-x86\_64.tar.gz**
	
	Linux (x86\_64): **ngless-0.0.0-Linux-x86\_64.tar.gz**

In this case, NGLess executable can be used directly and it's located at **ngless-*/bin**.


FAQ
--------
**During 'make' an error was reported saying : 'curses.h: No such file or directory.'. How do I fix it?**

You need to install the curses library which include routines for a terminal-independent method of updating character screens with reasonable optimization. 
The fix depends on the Operating System you are currently in.

Ubuntu::
	
	$ sudo apt-get install libncurses5-dev libncursesw5-dev
		
Fedora / RHEL / CentOS Linux::
	
	$ yum install ncurses-devel ncurses
	
	
**During 'make' an error was reported saying 'Error: SSE2 instruction set not enabled'. How do I fix it?**

This is a known problem when compiling the Burrows-Wheeler Aligner(BWA) tool when under a **32 bits** operating system. To fix it, you have to change the Makefile inside the directory **bwa-0.7.7/** in

	Line 3) CFLAGS= -g -Wall -O2 -msse -mmmx -msse2

	Line 6) DFLAGS= -DHAVE_PTHREAD
	
