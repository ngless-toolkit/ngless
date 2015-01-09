.. _Tests:

============
Tests
============

These are the required programs you must have installed to test the ngless tool

- GHC (version 7.6.3 or higher)
- Cabal (1.8.0.3 or higher)
- Git
- Python (with numpy and matplotlib)

Install Python libraries
------------------------
Depending on the operating system, there are multiple ways to install the required packages. **Make sure you have them installed before doing anything else!**

In case they aren't, there are a few steps depending on the operating system that you can use to install them.

Ubuntu
~~~~~~~~~
::

  $ sudo apt-get install build-essential python2.7-dev python-numpy python-matplotlib

RedHat, Fedora, CentOS
~~~~~~~~~~~~~~~~~~~~~~
::

  $ sudo yum groupinstall "Development Tools"
  $ sudo yum install python-devel numpy python-matplotlib
  
Mac
~~~~~~~~~~
Using **macports**, can be installed by running the following command::
  
  $ sudo port install py27-numpy py27-matplotlib


Steps
-------

Start by download latest NGLess version from Github.::

    $ git clone https://github.com/luispedro/ngless
    $ cd ngless

Then compite and run nglesstest by executing the following command.::

    $ make check
