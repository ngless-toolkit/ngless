{ checkMaterialization ? false }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
      name = "haskell-nix-snap";
      url = "https://github.com/input-output-hk/haskell.nix/archive/a7f031ac146666657824f3c4603bf87d3507c1a6.tar.gz";
      sha256 = "1p4rrq2apw2lnswj6km7ni9zc6gvr1ly03pkparyjslmcf5d57n4";
    };
  };

  haskellNix = import sources.haskellNix { };

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;

    ignoredPaths = ["tests" "docs" "run-tests.sh"];
in pkgs.haskell-nix.stackProject {
  name = "NGLess";
  src = (import ./sources-with-static-dependencies.nix) ;
  stack-sha256 = "0i2r2kiacb3n0m6yjfzil5rh6dhdq9jjvy7a2czidr4dnnp0swxn";
  materialized = ./sources-with-static-dependencies.materialized;
  inherit checkMaterialization;
}

