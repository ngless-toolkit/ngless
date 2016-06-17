{ mkDerivation, aeson, ansi-terminal, async, base, bytestring
, bzlib, bzlib-conduit, conduit, conduit-combinators, conduit-extra
, configurator, containers, convertible, data-default, deepseq
, directory, double-conversion, edit-distance, errors, extra
, file-embed, filemanip, filepath, hashable, hashtables
, http-client, http-conduit, HUnit, IntervalMap, MissingH, mtl
, network, old-locale, optparse-applicative, parallel, parsec
, primitive, process, QuickCheck, random, resourcet, stdenv, stm
, stm-chans, stm-conduit, strict, tar, template-haskell
, test-framework, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, text, time, transformers, unix, vector
, vector-algorithms, yaml, zlib
, ghc
# , fetchurl
, fetchFromGitHub
, makeWrapper
, pkgs
}:
let
  isGHC8 = stdenv.lib.versionOlder "8.0" ghc.version;
in
mkDerivation {
  pname = "NGLess";
  version = "0.0.0";
  src = fetchFromGitHub {
    owner = "luispedro";
    repo = "ngless";
    rev = "9c21913ee710119dfa1cb07098a00019dc35e98e";
    sha256 = "12dqm5pa101vxn492b88j9an8la52l8m4sih2w1fsrgn1cz6aad8";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal async base bytestring bzlib bzlib-conduit
    conduit conduit-combinators conduit-extra configurator containers
    convertible data-default deepseq directory double-conversion
    edit-distance errors extra file-embed filemanip filepath hashable
    hashtables http-client http-conduit IntervalMap MissingH mtl
    network old-locale optparse-applicative parallel parsec primitive
    process random resourcet stm stm-chans stm-conduit strict tar
    template-haskell text time transformers unix vector
    vector-algorithms yaml zlib
    pkgs.wget
    pkgs.m4
    pkgs.samtools
    pkgs.bwa
    makeWrapper
  ];
  testHaskellDepends = [
    aeson ansi-terminal async base bytestring bzlib bzlib-conduit
    conduit conduit-combinators conduit-extra configurator containers
    convertible data-default deepseq directory double-conversion
    edit-distance errors extra file-embed filemanip filepath hashable
    hashtables http-client http-conduit HUnit IntervalMap MissingH mtl
    network old-locale optparse-applicative parallel parsec primitive
    process QuickCheck random resourcet stm stm-chans stm-conduit
    strict tar template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th text time transformers
    unix vector vector-algorithms yaml zlib
  ];
  configureFlags = "-f-embed";
  license = stdenv.lib.licenses.mit;

  jailbreak = isGHC8;
  # Work around a bug in jailbreak: https://github.com/peti/jailbreak-cabal/issues/11
  patches = if isGHC8 then [ ./jailbreak-workaround.patch ] else null;


  prePatch = ''
    m4 NGLess.cabal.m4 > NGLess.cabal
  '';


  postBuild = ''
    make modules
  '';

  postInstall = ''
    mkdir -p $out/share/ngless/data
    mkdir -p $out/share/ngless/bin
    cp -pr Modules $out/share/ngless/data
    wrapProgram $out/bin/ngless \
        --set NGLESS_SAMTOOLS_BIN ${pkgs.samtools}/bin/samtools \
        --set NGLESS_BWA_BIN ${pkgs.bwa}/bin/bwa
  '';
}
