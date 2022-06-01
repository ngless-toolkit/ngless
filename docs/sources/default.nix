#!/usr/bin/env nix-shell

with (import (builtins.fetchTarball {
  name = "nixml-stable-19.04";
  url = https://github.com/nixos/nixpkgs/archive/37694c8cc0e9ecab60d06f1d9a2fd0073bcc5fa3.tar.gz;
  sha256 = "1ibjg6ln2y764wfg0yd0v055jm4xzhmxy5lfz63m6jr3y16jdmzb";
}) {});


let
    envname = "rtfd";
    python = python36Full;
    pyp = pkgs.python36Packages;
    guzzle = python.pkgs.buildPythonPackage rec {
        pname = "guzzle_sphinx_theme";
        version = "0.1";
        name = "python-${pname}-${version}";

        src = pkgs.fetchgit {
            url = "https://github.com/guzzle/guzzle_sphinx_theme";
            rev = "d351448ac36933ce27ac3ef0fa813d286dc3f760";

            sha256 = "1mvbhpb3274chj9hbi7wvbmdri84l405im3h9k1gashwhxy5cvs9";
          };
        buildInputs = with python.pkgs; [ sphinx ];

        doCheck = false;

        meta = {
          homepage = "https://github.com/guzzle/guzzle_sphinx_theme";
          description = "Sphinx theme";
          maintainers = with pkgs.maintainers; [ fridh ];
        };
      };
in

pyp.buildPythonPackage {
  name = "${envname}-env";
  buildInputs = with pyp; [
     python
     stdenv
     virtualenv
     recommonmark
     setuptools
     numpydoc
     readline
     sphinx
     guzzle
  ];
  src = ./.;
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    make html
  '';
  installPhase = ''
    cp -pr _build/html $out
  '';
}

