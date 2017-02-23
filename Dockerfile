# Based on https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack
# USE ALPINE LINUX
FROM alpine
RUN apk update

RUN echo "https://s3-us-west-2.amazonaws.com/alpine-ghc/8.0" >> /etc/apk/repositories
ADD https://raw.githubusercontent.com/mitchty/alpine-ghc/master/mitch.tishmack%40gmail.com-55881c97.rsa.pub \
    /etc/apk/keys/mitch.tishmack@gmail.com-55881c97.rsa.pub
RUN apk update


# We add vim to get xxd
RUN apk add \
    alpine-sdk \
    bzip2-dev \
    ca-certificates \
    ghc \
    git \
    gmp-dev \
    m4 \
    make \
    vim \
    xz \
    zlib-dev

# GRAB A RECENT BINARY OF STACK
ADD https://s3.amazonaws.com/static-stack/stack-1.1.2-x86_64 /usr/local/bin/stack
RUN chmod 755 /usr/local/bin/stack
RUN mkdir -p /usr/src/

# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/5.3.0/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o

# RUN git clone --depth=10 https://github.com/luispedro/ngless  /usr/src/ngless
COPY ./ngless /usr/src/ngless
WORKDIR /usr/src/ngless
RUN m4 NGLess.cabal.m4 > NGLess.cabal
RUN stack setup
RUN stack --local-bin-path /usr/local/bin install --dependencies-only
RUN make static
#RUN stack --local-bin-path /usr/local/bin install --ghc-options '-optl-static -fPIC'
