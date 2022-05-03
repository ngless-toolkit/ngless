#!/usr/bin/env bash

set -ve

nix build -f release.nix NGLess.components.exes.ngless --out-link ngless-dyn

nix build -f release.nix projectCross.musl64.hsPkgs.NGLess.components.exes.ngless --out-link ngless-static

