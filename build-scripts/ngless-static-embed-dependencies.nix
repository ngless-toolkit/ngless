{ checkMaterialization ? false }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
        name = "haskell-nix-snap";
        url = "https://github.com/input-output-hk/haskell.nix/archive/fadf9227afcdd93eedc656ba380f6a55d08fa650.tar.gz";
        sha256 = "0bxk9gdw393gm9h2vr9lh70488n7pzj060ik1s2q54d2ydb44xfn";
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
  stack-sha256 = "159b7sfkjfv5m0ppfb9lkr4vcplb152s3b7j6g9jhg9kcd9w0rmi";
  materialized = ./sources-with-static-dependencies.materialized;
  inherit checkMaterialization;
}

