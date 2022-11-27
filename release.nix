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
  stack-sha256 = "04zdpv5z7ns12c233h6f1365wd21inykkkbriz2rai1sbfvzhv2m";
  inherit checkMaterialization;
}

