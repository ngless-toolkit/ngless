{
  extras = hackage:
    {
      packages = {
        NGLess = ./NGLess.nix;
        int-interval-map = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-19.4";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = {}; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }