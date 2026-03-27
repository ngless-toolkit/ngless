{ checkMaterialization ? false }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
      name = "haskell-nix-snap";
      url = "https://github.com/input-output-hk/haskell.nix/archive/8cbc9c467e3990204ebbafba29bd53f6bea60757.tar.gz";
      sha256 = "1bciwshiax23sbgmzzkam4amswphxg7v3g5032c5z5dvnhwjd097";
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
  materialized = ./sources-with-static-dependencies.materialized;
  stack-sha256 = "1im1fa40x9y0985fkmkcdc7y8rqw77bfgmll6vvv3w2nxlb23sh9";
  inherit checkMaterialization;
}

