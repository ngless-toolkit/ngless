=====================================
NGLess: NGS Processing with Less Work
=====================================

Requirements:

GHC version 7.6.3 or higher
Cabal 1.8.0.3 or higher

Both can be installed via haskell-plataform package.

=====
Cabal
=====

Cabal by default comes with a old version installed.

Start by running ::
	
	cabal version

If the version is equal or higher than 1.8.0.3 you are ready to install NGLess and no more steps are required! 

Otherwise continue this installation process wich will update your cabal version. Start by running the following command:

	cabal install cabal-install --prefix=/usr/local

This command should have installed everything and since '/usr/local' is by default in $PATH you should be ready to go.

To be sure, run again ::

	cabal version

Check if the new version is higher than 1.8.0.3, if it isn't one of the following problems might be occurring::

	1. /usr/local is not in your $PATH

	2. A older version of cabal is installed in some directory which comes first than '/usr/local' in your $PATH variable.	


Installation steps:

1. make
2. make install prefix=dir (default is /usr/local)

More options:


