# Installation

## Bioconda (binary)

The recommended way to install NGLess is through
[bioconda](https://bioconda.github.io):

    conda install -c bioconda ngless 

This will install the most recent released version

## Beta releases (via pixi)

Pre-release/beta builds are published as `.conda` packages on the [GitHub
releases page](https://github.com/ngless-toolkit/ngless/releases) before they
reach bioconda. The easiest way to install one, together with the external tools
that NGLess drives (`bwa`, `samtools`, `minimap2`, `megahit`, `prodigal`), is
with [pixi](https://pixi.sh) and a manifest that pins the beta package by URL.

pixi only reads a manifest named exactly `pixi.toml`, so create a directory for
the install and put the manifest there as `pixi.toml` (a ready-to-use copy ships
in the [NGLess
repository](https://github.com/ngless-toolkit/ngless/blob/master/pixi_install_ngless.toml)):

    [workspace]
    channels = ["conda-forge", "https://conda.anaconda.org/bioconda"]
    name = "ngless_test"
    platforms = ["linux-64"]
    version = "0.1.0"

    [dependencies]
    prodigal = ">=2.6.3,<3"
    megahit = ">=1.2.9,<2"
    samtools = ">=1.23.1,<2"
    minimap2 = ">=2.31,<3"
    bwa = ">=0.7.19,<0.8"
    ngless = { url = "https://github.com/ngless-toolkit/ngless/releases/download/v1.6.0-beta2/ngless-1.6.beta2-ha35fb5c_0.conda" }

Then, from that directory, install and run it with:

    pixi install
    pixi run ngless --version

Update the `url` to point at the beta release you want to install.

### Docker

Alternatively, a docker container with NGLess is available at
[docker hub](https://hub.docker.com/r/nglesstoolkit/ngless):

    docker run -v $PWD:/workdir -w /workdir -it nglesstoolkit/ngless:1.4.2 ngless --version

Adapt the mount flags (``-v``) as needed. You can use the `latest` tag to get a
more up to date version as well.


## Linux (binary)

You can download a [statically linked version of NGless
1.4.2](https://github.com/ngless-toolkit/ngless/releases/download/v1.4.2/NGLess-v1.4.2-Linux-static-full).

This should work across a wide range of Linux versions (please
[report](https://github.com/ngless-toolkit/ngless/issues) any issues you encounter):

    curl -L -O https://github.com/ngless-toolkit/ngless/releases/download/v1.4.2/NGLess-v1.4.2-Linux-static-full
    chmod +x NGLess-v1.4.2-Linux-static-full
    ./NGLess-v1.4.2-Linux-static-full

This downloaded file bundles bwa, samtools and megahit (also statically linked).

## From source

Since version 1.6, NGLess is written in [Rust](https://www.rust-lang.org/) and
builds with a standard [Cargo](https://doc.rust-lang.org/cargo/) toolchain (see
[the Rust implementation page](rust.md) for background).

The following sequence of commands should download and build the software

    git clone https://github.com/ngless-toolkit/ngless
    cd ngless
    cargo build --release

This produces the binary at `target/release/ngless`. The first build takes a
while as Cargo downloads and compiles all dependencies; subsequent builds are
much faster.

The external tools that NGLess drives (`bwa`, `samtools`, `minimap2`, `megahit`,
`prodigal`) are **not** bundled by a source build: they must be available on your
`$PATH` (or pointed to via the `NGLESS_<TOOL>_BIN` environment variables, e.g.
`NGLESS_SAMTOOLS_BIN`). The versions pinned for testing are listed in `pixi.toml`.

## Cargo commands

- `cargo build --release`: compile the optimized `ngless` binary
- `cargo test`: run the unit tests
- `cargo fmt --all -- --check`: check formatting (enforced in CI)

The functional/parity test suite is run with `run-tests.sh`, pointed at the build
via the `NGLESS_BIN` environment variable:

    NGLESS_BIN=target/release/ngless ./run-tests.sh

