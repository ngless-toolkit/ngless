# Installation

## Bioconda (binary)

The recommended way to install NGLess is through
[bioconda](https://bioconda.github.io):

    conda install -c bioconda ngless 

This will install the most recent released version

## Using pixi

[pixi](https://pixi.sh) is an alternative to conda that installs NGLess into a
self-contained, per-project environment. The external tools that NGLess drives
(`bwa`, `samtools`, `minimap2`, `megahit`, `prodigal`) are dependencies of the
bioconda package, so they are pulled in automatically.

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
    ngless = ">=1.6.0,<2"

Then, from that directory, install and run it with:

    pixi install
    pixi run ngless --version

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

