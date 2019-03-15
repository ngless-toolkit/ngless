FROM ubuntu:xenial

LABEL maintainer "alves.rjc+docker@gmail.com"

ARG GHC_VERSION=8.6.4

RUN apt update && \
    apt install -y \
    build-essential \
    cabal-install \
    coreutils \
    curl \
    git \
    libbz2-dev \
    libcairo2-dev \
    libgif-dev \
    libjpeg-dev \
    software-properties-common \
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
    rm -rf /var/lib/apt/lists

# add-apt-repository is installed by software-properties-common above
RUN add-apt-repository ppa:hvr/ghc-wsl -y
RUN apt-get update && \
    apt-get install -y ghc-${GHC_VERSION} && \
    apt clean && \
    rm -rf /var/lib/apt/lists

RUN wget -qO- https://get.haskellstack.org/ | sh && \
    chmod 755 /usr/local/bin/stack

# Set encoding to UTF-8 and PATH to find GHC and cabal/stack-installed binaries.
#

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/root/.cabal/bin:/root/.local/bin:/opt/ghc/$GHC_VERSION/bin:$PATH

# Build ngless dependencies to save time during build
RUN git clone --depth=2 https://gitlab.com/ngless/ngless && \
    cd ngless && \
    stack setup --system-ghc && \
    STACKOPTS="--system-ghc --only-dependencies" make ngless && \
    STACKOPTS="--system-ghc --only-dependencies" make tests && \
    cd .. && \
    rm -rf ngless
