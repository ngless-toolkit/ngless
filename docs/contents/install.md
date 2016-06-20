# Installation

**NOTE**: As of Apr 2016, ngless is available only as a pre-release to allow
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

## With Nix

If you use [nix](http://nixos.org), you can easily build and install ngless
using it (these scripts also install all necessary dependencies):

    nix-env -i -f https://github.com/luispedro/ngless-nix/archive/master.tar.gz

This should download the nix scripts and build ngless.

If you prefer, you can also first clone the repository:

    git clone https://github.com/luispedro/ngless-nix
    cd ngless-nix
    # inspect the default.nix & ngless.nix files if you wish
    nix-env -i -f .

## Make targets

The following are targets in the Makefile.

make - compiles NGLess and haskell dependencies
clean - remove local generated files by compilation
check - run tests
bench - run benchmarks

