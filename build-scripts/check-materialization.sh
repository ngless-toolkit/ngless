#!/usr/bin/env bash

for nixfile in release.nix build-scripts/ngless-static-embed-dependencies.nix ; do
    nix build -f ${nixfile} NGLess.components.exes.ngless --arg checkMaterialization true
done
