{
  extras = hackage:
    {
      packages = {
        NGLess = ./NGLess.nix;
        int-interval-map = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-20.1";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = {}; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }