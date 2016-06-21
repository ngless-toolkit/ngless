name:                NGLess
version:             0.0.0
-- synopsis:
-- description:
license:             MIT
license-file:        COPYING
author:              Luis Pedro Coelho and Paulo Monteiro
maintainer:          luis@luispedro.org
-- copyright:
category:            Domain Specific Language
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.22

Flag embed
    Description: Embed bwa/samtools
    Default: False

define(`BUILD_DEPENDS',
    `aeson >= 0.9,
    ansi-terminal == 0.6.*,
    async,
    base,
    bytestring == 0.10.*,
    bzlib == 0.5.*,
    bzlib-conduit == 0.2.*,
    optparse-applicative == 0.12.*,
    conduit >= 1.2 && < 1.3,
    conduit-extra == 1.1.* && >= 1.1.12,
    conduit-combinators == 1.0.*,
    configurator == 0.3.*,
    containers == 0.5.*,
    convertible == 1.1.*,
    double-conversion == 2.0.*,
    data-default,
    deepseq >= 1.3,
    directory == 1.2.*,
    edit-distance == 0.2.*,
    errors == 2.1.*,
    extra == 1.4.*,
    filemanip == 0.3.6.*,
    filepath >= 1.3,
    file-embed >= 0.0.8,
    hashable == 1.2.*,
    hashtables == 1.2.*,
    -- happy >= 1.17,
    http-conduit == 2.1.*,
    http-client == 0.4.*,
    IntervalMap == 0.5.*,
    mtl == 2.2.*,
    MissingH >= 1.3,
    network == 2.6.*,
    parallel == 3.2.*,
    parsec == 3.1.*,
    primitive == 0.6.*,
    process == 1.2.* && >= 1.2.3,
    old-locale,
    random == 1.1.*,
    strict == 0.3.*,
    stm == 2.4.*,
    stm-chans == 3.0.*,
    stm-conduit >= 2.7,
    resourcet >= 1.1 && < 1.2,
    tar == 0.5.*,
    template-haskell,
    text == 1.2.*,
    time >= 1.5,
    transformers == 0.4.*,
    unix == 2.7.*,
    vector == 0.11.*,
    vector-algorithms,
    yaml,
    zlib')

define(`BASE_CONFIG',
  `C-sources: NGLess/Dependencies/embedded.c NGLess/Data/FastQ.c
  default-extensions:  BangPatterns, OverloadedStrings, LambdaCase, TupleSections
  other-extensions:    DeriveDataTypeable, TemplateHaskell
  hs-source-dirs: NGLess/
  default-language:    Haskell2010
  if impl(ghc >= 8.0)
    ghc-options: -Wall -fwarn-tabs -fno-warn-missing-signatures -threaded -O2 -rtsopts "-with-rtsopts=-A64m -n4m -H"
  else
    ghc-options: -Wall -fwarn-tabs -fno-warn-missing-signatures -threaded -O2 -rtsopts "-with-rtsopts=-A64m -n4m -H -qg"
  if !flag(Embed)
      CC-Options: -DNO_EMBED_SAMTOOLS_BWA
')

executable ngless
  main-is: Main.hs
  BASE_CONFIG
  build-depends:
    BUILD_DEPENDS

Test-Suite nglesstest
  type:         exitcode-stdio-1.0
  main-is:      Tests.hs
  BASE_CONFIG
  build-depends:
    BUILD_DEPENDS,
    HUnit == 1.3.*,
    test-framework == 0.8.*,
    test-framework-hunit,
    test-framework-quickcheck2,
    test-framework-th,
    QuickCheck == 2.8.*

benchmark nglessbench
  type:         exitcode-stdio-1.0
  main-is:      Bench.hs
  BASE_CONFIG
  build-depends:
    BUILD_DEPENDS,
    criterion
