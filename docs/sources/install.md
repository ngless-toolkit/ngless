# Installation

## Bioconda (binary)

The recommended way to install NGLess is through
[bioconda](https://bioconda.github.io):

    conda install -c bioconda ngless 

This will install the most recent released version

### Docker

Alternatively, a docker container with NGLess is available at
[docker hub](https://hub.docker.com/r/nglesstoolkit/ngless):

    docker run -v $PWD:/workdir -w /workdir -it nglesstoolkit/ngless:1.4.1 ngless --version

Adapt the mount flags (``-v``) as needed. You can use the `latest` tag to get a
more up to date version as well.


## Linux (binary)

You can download a [statically linked version of NGless
1.4.1](https://github.com/ngless-toolkit/ngless/releases/download/v1.4.1/NGLess-v1.4.1-Linux-static-full).

This should work across a wide range of Linux versions (please
[report](https://github.com/ngless-toolkit/ngless/issues) any issues you encounter):

    curl -L -O https://github.com/ngless-toolkit/ngless/releases/download/v1.4.1/NGLess-v1.4.1-Linux-static-full
    chmod +x NGLess-v1.4.1-Linux-static-full
    ./NGLess-v1.4.1-Linux-static-full

This downloaded file bundles bwa, samtools and megahit (also statically linked).

## From source

[Stack](https://docs.haskellstack.org/en/stable/README.html) is the simplest way
to install the necessary requirements.

The following sequence of commands should download and build the software

    git clone https://github.com/ngless-toolkit/ngless
    cd ngless
    make


The first time you run this, it will take a while as it will download all
dependencies. After this ngless is ready to use and subsequent builds will be
much faster.

## Make targets

The following are targets in the Makefile.

- `make`: compiles NGLess and haskell dependencies
- `clean`: remove local generated files by compilation
- `check`: run tests
- `bench`: run benchmarks

