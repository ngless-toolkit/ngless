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
    ansi-terminal,
    async,
    atomic-write >= 0.2,
    base,
    bytestring,
    bytestring-lexing,
    optparse-applicative,
    conduit,
    conduit-extra >= 1.1.12,
    conduit-combinators,
    configurator,
    containers,
    convertible,
    data-default,
    deepseq >= 1.3,
    directory,
    edit-distance >= 0.2,
    either,
    errors >= 2.1,
    extra >= 1.4,
    filemanip >= 0.3.6,
    filepath >= 1.3,
    file-embed >= 0.0.8,
    gitrev,
    hashable,
    hashtables,
    -- happy >= 1.17,
    http-conduit,
    http-client,
    IntervalMap >= 0.5,
    monad-control,
    mtl >= 2.2,
    MissingH >= 1.3,
    network,
    parsec >= 3.1,
    primitive >= 0.6,
    process >= 1.2.3,
    old-locale,
    random,
    safe,
    safeio >= 0.0.2,
    strict,
    stm,
    stm-chans,
    stm-conduit >= 2.7,
    resourcet >= 1.1,
    tar >= 0.5,
    template-haskell,
    text >= 1.2,
    time >= 1.5,
    transformers,
    transformers-base,
    vector >= 0.11,
    vector-algorithms,
    yaml,
    zlib')

define(`BUILD_MODULES',
    `BuiltinFunctions
    BuiltinModules.Argv
    BuiltinModules.AsReads
    BuiltinModules.Assemble
    BuiltinModules.Checks
    BuiltinModules.Readlines
    BuiltinModules.Remove
    CmdArgs
    Configuration
    Data.FastQ
    Data.GFF
    Data.Sam
    Dependencies.Embedded
    ExternalModules
    FileManagement
    FileOrStream
    Hooks
    Interpret
    Interpretation.Count
    Interpretation.FastQ
    Interpretation.Map
    Interpretation.Select
    Interpretation.Substrim
    Interpretation.Unique
    Interpretation.Write
    JSONScript
    Language
    Modules
    NGLess
    NGLess.NGError
    NGLess.NGLEnvironment
    Network
    Output
    Parse
    ReferenceDatabases
    StandardModules.Batch
    StandardModules.Example
    StandardModules.Mappers.Bwa
    StandardModules.Mappers.Soap
    StandardModules.Mocat
    StandardModules.NGLStdlib
    StandardModules.Parallel
    StandardModules.Samtools
    StandardModules.Soap
    Tokens
    Transform
    Types
    Utils.Conduit
    Utils.Batch
    Utils.FindModules
    Utils.IntGroups
    Utils.LockFile
    Utils.ProgressBar
    Utils.Samtools
    Utils.Suggestion
    Utils.Utils
    Utils.Vector
    Validation
    ValidationIO')

define(`TEST_MODULES',
    `Tests.Count
    Tests.FastQ
    Tests.IntGroups
    Tests.NGLessAPI
    Tests.Parse
    Tests.Select
    Tests.Types
    Tests.Utils
    Tests.Validation
    Tests.Vector
    Utils.Here')

define(`BENCH_MODULES',
    `Tests.Utils')

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
  if os(windows)
    cpp-options: -DWINDOWS
  if !os(windows)
    build-depends:
      bzlib,
      bzlib-conduit,
      double-conversion,
      unix
  if !flag(Embed)
    CC-Options: -DNO_EMBED_SAMTOOLS_BWA
    cpp-options: -DNO_EMBED_SAMTOOLS_BWA
  else
    ld-options: -static -pthread
')

executable ngless
  main-is: Main.hs
  BASE_CONFIG
  build-depends:
    BUILD_DEPENDS
  other-modules:
    BUILD_MODULES

Test-Suite nglesstest
  type:         exitcode-stdio-1.0
  main-is:      Tests.hs
  BASE_CONFIG
  build-depends:
    BUILD_DEPENDS,
    HUnit >= 1.3,
    test-framework >= 0.8,
    test-framework-hunit,
    test-framework-quickcheck2,
    test-framework-th,
    QuickCheck >= 2.8
  other-modules:
    BUILD_MODULES
    TEST_MODULES

benchmark nglessbench
  type:         exitcode-stdio-1.0
  main-is:      Bench.hs
  BASE_CONFIG
  build-depends:
    BUILD_DEPENDS,
    HUnit >= 1.3,
    criterion
  other-modules:
    BUILD_MODULES,
    BENCH_MODULES
