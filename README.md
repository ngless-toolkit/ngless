# NGLess: NGS Processing with Less Work

Ngless is a domain-specific language for NGS (next-generation sequencing data)
processing.

[![Build Status](https://travis-ci.org/luispedro/ngless.svg?branch=master)](https://travis-ci.org/luispedro/ngless)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/hyperium/hyper/master/LICENSE)
[![Install with Bioconda](https://anaconda.org/bioconda/ngless/badges/installer/conda.svg)](https://anaconda.org/bioconda/ngless)
[![Install with Bioconda](https://anaconda.org/bioconda/ngless/badges/downloads.svg)](https://anaconda.org/bioconda/ngless)
[![Join the chat at https://gitter.im/luispedro/ngless](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/luispedro/ngless?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


**Note**: This is *pre-release* software, currently in beta (testing) It is
stable enough to use, but there may still be some (minor) changes before an
official release. For questions, you can also use the [ngless mailing
list](https://groups.google.com/forum/#!forum/ngless).

## Example

    ngless "0.7"
    input = fastq(['ctrl1.fq','ctrl2.fq','stim1.fq','stim2.fq'])
    input = preprocess(input) using |read|:
        read = read[5:]
        read = substrim(read, min_quality=26)
        if len(read) < 31:
            discard

    mapped = map(input,
                    reference='hg19')
    write(count(mapped, features=['gene']),
            ofile='gene_counts.csv',
            format={csv})

## Installing

See the [install documentation](http://ngless.embl.de/install.html) for more
information.

### Bioconda

The recommended way to install NGLess is through
[bioconda](http://bioconda.github.io):

    conda install -c bioconda ngless 

### Docker

Alternatively, a docker container with NGLess is available at
[biocontainers](https://quay.io/repository/biocontainers/ngless):

    docker run -v $PWD:/workdir -w /workdir -it quay.io/biocontainers/ngless:0.7.0--py35_0 ngless --version

Adapt the mount flags (``-v``) as needed.

### Linux

You can get a [statically linked version of
NGless 0.7.0](http://ngless.embl.de/releases/ngless-0.7.0-Linux64) or a [nighly
build of the latest development
code](https://gitlab.com/ngless/ngless/builds/artifacts/master/raw/bin/ngless?job=build-and-test-ubuntu).
This should work across a wide range of Linux versions (please
[report](https://github.com/luispedro/ngless/issues) any issues you encounter):

    curl -O http://ngless.embl.de/releases/ngless-0.7.0-Linux64
    chmod +x ngless-0.7.0-Linux64
    ./ngless-0.7.0-Linux64

This download bundles bwa, samtools and megahit (also statically linked).

If you want to try one of ngless' builtin modules (motus, specI, ...) you can
download [the full nighly build zip
file](https://gitlab.com/ngless/ngless/builds/artifacts/master/download?job=build-and-test-ubuntu)
which includes them.

### From Source

Installing/compiling from source is also possible. Clone
[http://github.com/luispedro/ngless](http://github.com/luispedro/ngless)

#### Dependencies

[stack](http://docs.haskellstack.org/en/stable/README/) is highly recommended.
Install it and running `make` should (1) download all dependencies with the
correct versions and (2) build ngless. It will perform this task in its own
sandbox so it will not interfere with any other work.

You should also have `gcc` installed (or another C-compiler).

The following sequence of commands should download and build the software

    git clone https://github.com/luispedro/ngless
    cd ngless
    make

To install, you can use the following command (replace `<PREFIX>` with
the directory where you wish to install, default is `/usr/local`):

    make install prefix=<PREFIX>

## More information

- [Full documentation](http://ngless.embl.de/)
- [Frequently Asked Questions (FAQ)](http://ngless.embl.de/faq.html)
- [ngless mailing list](https://groups.google.com/forum/#!forum/ngless)
- [What's new log](http://ngless.embl.de/whatsnew.html)

## Authors

- [Luis Pedro Coelho](http://luispedro.org) (email: [coelho@embl.de](mailto:coelho@embl.de)) (on twitter: [@luispedrocoelho](https://twitter.com/luispedrocoelho))
- Paulo Monteiro
-  Renato Alves
- [Ana Teresa Freitas](http://web.tecnico.ulisboa.pt/ana.freitas/)
-  Peer Bork

