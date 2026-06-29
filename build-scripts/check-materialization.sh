#!/usr/bin/env bash

for nixfile in release.nix ; do
    nix build -f ${nixfile} NGLess.components.exes.ngless --arg checkMaterialization true
done
