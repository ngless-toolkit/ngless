{ checkMaterialization ? false }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
      name = "haskell-nix-snap";
      url = "https://github.com/input-output-hk/haskell.nix/archive/17e5f35d56c57b20ba2397010fcd4032fb6acc2b.tar.gz";
      sha256 = "0b3pwrib289bhidv1q27k6k53zwyahdffi2ynww19qnviqwlm21a";
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

    ignoredPaths = ["tests" "docs" "run-tests.sh" ".github"];
in pkgs.haskell-nix.stackProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.lib.cleanSourceWith {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "NGLess";
      src = ./.;
    };
    # ignore paths that change frequently, but do not contribute to the result
    filter = path: type: let baseName = baseNameOf (toString path); in !(pkgs.lib.elem baseName ignoredPaths);
  };
  materialized = ./build-scripts/release.materialized;
  stack-sha256 = "0lpv5h8zzvmdamw0sxxnqaj919b3wl4b3zmc3d1z38ckpgpxi16b";
  inherit checkMaterialization;
}

