#!/usr/bin/env nix-shell

with (import (builtins.fetchTarball {
  name = "nixpksg-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/e17b079679d75b59934e02664fec6b214b2779e9.tar.gz";
  sha256 = "026q0k70d6k3gnsaxfclxp9130v253nr0zlrf2xiv7r6cz8ky569";
}) {});

let
    envname = "rtfd";
    python = python39Full;
    pyp = pkgs.python39Packages;
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
     myst-parser
     setuptools
     numpydoc
     readline
     sphinx
     guzzle
  ];
  src = ./.;
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    make html SPHINXBUILD=${sphinx}/bin/sphinx-build
  '';
  installPhase = ''
    cp -pr _build/html $out
  '';
}

