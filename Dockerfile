# Based on https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack
# USE ALPINE LINUX
FROM alpine:edge
RUN echo '@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories
RUN apk update && apk add \
    alpine-sdk \
    bash \
    bzip2-dev \
    ca-certificates \
    cabal@testing \
    file \
    ghc-dev@testing \
    ghc@testing \
    git \
    gmp-dev \
    libffi-dev \
    libgcc \
    linux-headers \
    m4 \
    make \
    openssh \
    openssh-client \
    vim \
    xz \
    xz-dev \
    zlib-dev
# We add vim to get xxd

# GRAB A RECENT BINARY OF STACK
RUN wget -qO- https://get.haskellstack.org/ | sh

RUN chmod 755 /usr/local/bin/stack
RUN mkdir -p /usr/src/

RUN git clone --depth=10 https://github.com/ngless-toolkit/ngless /usr/src/ngless
WORKDIR /usr/src/ngless

# This will have all make calls use the ghc installed above
# Build dependencies in a separate step to avoid a full rebuild on ngless compile failure
ENV STACKOPTS="--system-ghc --only-dependencies"
RUN make static

ENV STACKOPTS="--system-ghc"
RUN make static
RUN stack --local-bin-path /usr/local/bin install $STACKOPTS --ghc-options '-optl-static -optl-pthread' --flag NGLess:embed
