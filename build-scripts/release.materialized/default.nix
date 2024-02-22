{
  extras = hackage:
    {
      packages = {
        "conduit-algorithms" = (((hackage.conduit-algorithms)."0.0.14.0").revisions).default;
        NGLess = ./NGLess.nix;
        int-interval-map = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-22.6";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = {}; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }