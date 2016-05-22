with (import <nixpkgs> {});

stdenv.mkDerivation {

  name = "nglessEnv";

  buildInputs = with haskell.packages.lts-5_12; [
    ghc-mod
    hoogle
    hlint
    stack
    ghc
    m4
  ];
  propagatedBuildInputs = [
    zlib
    bzip2
  ];

  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${zlib.out}/lib"
      + " --extra-include-dirs=${zlib.out}/include"
  ;
  extraCmds = ''
    export LD_LIBRARY_PATH+=:${zlib.out}/lib
    export LD_LIBRARY_PATH+=:${bzip2.out}/lib
  '';
  shellHook = ''
    export LD_LIBRARY_PATH+=:${zlib.out}/lib
    export LD_LIBRARY_PATH+=:${bzip2.out}/lib
  '';
}
