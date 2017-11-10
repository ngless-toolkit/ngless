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
    zlib-dev && \
    wget -qO- https://get.haskellstack.org/ | sh && \
    chmod 755 /usr/local/bin/stack && \
    pip install tox

# Workaround for https://bugs.alpinelinux.org/issues/8098 (not yet fixed as of alpine package bash-4.4.12-1)
RUN cd /tmp && \
    wget https://ftp.gnu.org/gnu/bash/bash-4.3.30.tar.gz && \
    tar xf bash-4.3.30.tar.gz && \
    cd bash-4.3.30/ && \
    ./configure \
        --prefix=/usr \
        --bindir=/bin \
        --mandir=/usr/share/man \
        --infodir=/usr/share/info \
        --without-curses \
        --disable-nls \
        --disable-readline \
        --without-bash-malloc && \
    make -j8 && \
    make install && \
    rm -rf /tmp/bash*
# TODO Remove this once upstream package is fixed

# Build ngless dependencies to save time during build
RUN git clone https://gitlab.com/ngless/ngless && \
    cd ngless && \
    STACKOPTS="--system-ghc --only-dependencies" make ngless && \
    STACKOPTS="--system-ghc --only-dependencies" make tests && \
    cd .. && \
    rm -rf ngless
