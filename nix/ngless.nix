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
# , fetchurl
, fetchFromGitHub
, pkgs
}:
mkDerivation {
  pname = "NGLess";
  version = "0.0.0";
  src = fetchFromGitHub {
    owner = "luispedro";
    repo = "ngless";
    rev = "af5fa8be22e4da563004807bee4d92e806df6394";
    sha256 = "185jyg1462issnl6xv8v0f396bmmpvg22p53637pyrf9bck4gv39";
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

  preConfigure = ''
    m4 NGLess.cabal.m4 > NGLess.cabal
  '';

  postBuild = ''
    make modules
  '';

  postInstall = ''
    mkdir -p $out/share/ngless/data
    mkdir -p $out/share/ngless/bin
    cp -pr ${pkgs.samtools}/bin/samtools $out/share/ngless/bin/ngless-0.0.0-samtools
    cp -pr ${pkgs.bwa}/bin/bwa $out/share/ngless/bin/ngless-0.0.0-bwa
    cp -pr Modules $out/share/ngless/data
  '';
  
}
