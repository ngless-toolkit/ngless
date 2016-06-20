#!/usr/bin/env nix-shell

with import <nixpkgs> {};

let
    envname = "rtfd";
    python = python27Full;
    pyp = pkgs.python27Packages;
in

buildPythonPackage {
  name = "${envname}-env";
  buildInputs = with pyp; [
     python
     stdenv
     virtualenv
     recommonmark
     setuptools
     numpydoc
     readline
  ];
  src = ./.;
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];
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
  buildPhase = ''
    make html
  '';
  installPhase = ''
    cp -pr _build/html $out
  '';
}

