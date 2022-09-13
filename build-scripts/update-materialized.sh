#!/usr/bin/env bash

set -e
for nixfile in release.nix build-scripts/ngless-static-embed-dependencies.nix; do
    nix build -f ${nixfile} NGLess.project.stack-nix.passthru.updateMaterialized
    ./result
    nix build -f ${nixfile} NGLess.project.stack-nix.passthru.calculateMaterializedSha
    echo "stack-sha256 = \"$(./result)\";" > ${nixfile}.sha256
    echo "Updated ${nixfile}.sha256"
done

