FROM alpine:edge

LABEL maintainer "alves.rjc+docker@gmail.com"

RUN apk --no-cache add \
    alpine-sdk \
    bash \
    bzip2-dev \
    ca-certificates \
    cabal \
    coreutils \
    file \
    ghc-dev \
    ghc \
    git \
    gmp-dev \
    libffi-dev \
    libgcc \
    linux-headers \
    m4 \
    make \
    py2-pip \
    python2 \
    python2-dev \
    vim \
    xz \
    xz-dev \
    zlib-dev \
    zstd && \
    wget -qO- https://get.haskellstack.org/ | sh && \
    chmod 755 /usr/local/bin/stack && \
    pip install tox

# Build ngless dependencies to save time during build
RUN git clone --depth=2 https://gitlab.com/ngless/ngless && \
    cd ngless && \
    STACKOPTS="--system-ghc --only-dependencies" make ngless && \
    STACKOPTS="--system-ghc --only-dependencies" make tests && \
    cd .. && \
    rm -rf ngless
