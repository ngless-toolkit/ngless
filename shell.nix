with (import <nixpkgs> {});

stdenv.mkDerivation {

  name = "nglessEnv";

  buildInputs = with haskell.packages.lts-3_16; [
    ghc
    zlib
  ];
  propagatedBuildInputs = [
    zlib
    bzip2
  ];

  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${zlib}/lib" 
      + " --extra-include-dirs=${zlib}/include" 
  ;
  extraCmds = ''
    export LD_LIBRARY_PATH+=:${zlib}/lib
    export LD_LIBRARY_PATH+=:${bzip2}/lib
  '';
  shellHook = ''
    export LD_LIBRARY_PATH+=:${zlib}/lib
    export LD_LIBRARY_PATH+=:${bzip2}/lib
  '';
}
