FROM ubuntu:xenial

LABEL maintainer "alves.rjc+docker@gmail.com"

RUN apt update && \
    apt install -y \
    build-essential \
    cabal-install \
    coreutils \
    curl \
    ghc \
    git \
    libbz2-dev \
    libcairo2-dev \
    libgif-dev \
    libjpeg-dev \
    liblzma-dev \
    m4 \
    pkg-config \
    python-dev \
    python-pip \
    vim \
    wget \
    xz-utils \
    zlib1g-dev \
    zstd && \
    apt clean && \
    rm -rf /var/lib/apt/lists && \
    wget -qO- https://get.haskellstack.org/ | sh && \
    chmod 755 /usr/local/bin/stack && \
    pip install tox

# Build ngless dependencies to save time during build
# We need to use GHC from stack here because ubuntu:latest ships with GHC-7
RUN git clone --depth=2 https://gitlab.com/ngless/ngless && \
    cd ngless && \
    stack setup && \
    STACKOPTS="--only-dependencies" make ngless && \
    STACKOPTS="--only-dependencies" make tests && \
    cd .. && \
    rm -rf ngless
