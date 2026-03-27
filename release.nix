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
  stack-sha256 = "1raq8s3r786j83yqsf3wi00qhxbmmwwvrs1fdr901ixa3v05gha7";
  inherit checkMaterialization;
  modules = [
    {
      packages.directory.flags.os-string = true;
      packages.unix.flags.os-string = true;
      packages.process.flags.os-string = true;
    }
  ];

}

