{ checkMaterialization ? false }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
        name = "haskell-nix-snap";
        url = "https://github.com/input-output-hk/haskell.nix/archive/fbd32c9c441fbd35a85049978bffb4b2eff2f04b.tar.gz";
        sha256 = "0ihjyf7k47z71zznrs1453vi7kl2p82da18wngjmnr7rjap1vjyi";
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
  stack-sha256 = "1acr6d1qdih192fkbwncc85874v27x848c4iqwfbvg68lcs4973j";
  materialized = ./sources-with-static-dependencies.materialized;
  inherit checkMaterialization;
}

