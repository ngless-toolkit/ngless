#!/usr/bin/env nix-shell

with import <nixpkgs> {};

let
    envname = "py27";
    python = python27Full;
    pyp = pkgs.python27Packages;


    htseq = buildPythonPackage rec {
      name = "htseq-0.6.1";

      buildInputs =  with pyp; [ stdenv numpy setuptools ];
      src = pkgs.fetchurl {
        url = "https://pypi.python.org/packages/source/H/HTSeq/HTSeq-0.6.1.tar.gz";
        md5= "b7f4f38a9f4278b9b7f948d1efbc1f05";
      };

    };


in

buildPythonPackage { 
  name = "${envname}-env";
  buildInputs = [
     git
     python
     htseq
     stdenv
     zsh
    ];
   pythonPath = with pyp; [
     numpy
     ipython
     readline
     htseq
  ];
  src = null;
  # When used as `nix-shell --pure`
  shellHook = ''
  export NIX_ENV="[${envname}] "
  exec zsh
  '';
  # used when building environments
  extraCmds = ''
  export NIX_ENV="[${envname}] "
  exec zsh
  '';
}

