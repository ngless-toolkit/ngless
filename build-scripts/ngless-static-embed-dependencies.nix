{ checkMaterialization ? false }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
      name = "haskell-nix-snap";
      url = "https://github.com/input-output-hk/haskell.nix/archive/c689f01730e5b6c6c16d3947a15689569844c38c.tar.gz";
      sha256 = "09lw2419a5dd9g0ja31hjfqf6d4bzcgr5mrqx0vrvlksmp7a1kzk";
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
  stack-sha256 = "1lcj0vvrsyfq2fj014hf6q8qlg0rxvc387wkpb4kgclkhkwx7jg3";
  materialized = ./sources-with-static-dependencies.materialized;
  inherit checkMaterialization;
}

