.. _Installation:

============
Installation
============

Automated
-----

From source
-----

Requirements
~~~~~~~~~~

Before starting, make sure you have an Internet connection!

These are the required programms you must have installed

- GHC (version 7.6.3 or higher)
- Cabal (1.8.0.3 or higher)
- Git

Both GHC and Cabal can be installed via the haskell-platform package.

Update cabal
~~~~~~~~~~

Cabal by default comes with a old version installed. Start by running::
	
	cabal --version

If the version is equal or higher than 1.8.0.3 you are ready to install NGLess
and no more steps are required!  Otherwise continue this installation process
wich will update your cabal version. Start by running the following commands::

	cabal update
	sudo cabal install cabal-install --prefix=/usr

These commands require **super user privilege** and install the correct version of cabal in '/usr' which is by
default in **$PATH**. In case you only have **user privileges** you can choose a **--prefix** anywhere else 
(that you have permissions) but **make sure that is in your $PATH environment**. 
After the installation you should be ready to go. To be sure, run again::

	cabal --version

Check if the new version is higher than 1.8.0.3. 

If it is, you are now ready to install NGLess.

If it isn't, one of the following problems might be occurring

	1) The path used as --prefix is not in your $PATH.

	2) A older version of cabal is installed in some directory which comes first than '/usr/local' in your $PATH variable.


Steps
~~~~~~~~~~

Start by download latest NGLess version from Github.

    1. git clone https://github.com/luispedro/ngless
    2. cd ngless

Then download and configure all NGLess dependencies by running the following command.

    2. make

This will take a while, so go ahead and make some tea! After the previous
command is completed (without errors) you are ready to install it wherever you
want it to be.

    3. make install prefix=dir (default dir is /usr/local)

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
