with (import <nixpkgs> {});

stdenv.mkDerivation {

  name = "nglessEnv";

  buildInputs = with haskell.packages.ghc802; [
    stack
    wget
    ghc
    m4
  ];
  propagatedBuildInputs = [
    zlib
    bzip2
    lzma
    samtools
    bwa
  ];

  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${zlib.out}/lib"
      + " --extra-lib-dirs=${bzip2.out}/lib"
      + " --extra-lib-dirs=${lzma.out}/lib"
      + " --extra-include-dirs=${zlib.out}/include"
      + " --extra-include-dirs=${bzip2.out}/include"
      + " --extra-include-dirs=${lzma.out}/include"
  ;
  extraCmds = ''
    export LD_LIBRARY_PATH+=:${zlib.out}/lib
    export LD_LIBRARY_PATH+=:${bzip2.out}/lib
    export LD_LIBRARY_PATH+=:${lzma.out}/lib
  '';
  shellHook = ''
    export LD_LIBRARY_PATH+=:${zlib.out}/lib
    export LD_LIBRARY_PATH+=:${bzip2.out}/lib
    export LD_LIBRARY_PATH+=:${lzma.out}/lib

    export NGLESS_SAMTOOLS_BIN=${samtools}/bin/samtools
    export NGLESS_BWA_BIN=${bwa}/bin/bwa
  '';
}
