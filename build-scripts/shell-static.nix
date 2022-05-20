let
 release = import ./build-static-embedded-depencies.nix;
 pkgs = release.pkgs;

in pkgs.mkShell {

  name = "nglessEnv";

  nativeBuildInputs = [
    release.projectCross.musl64.hsPkgs.NGLess.components.exes.ngless
    pkgs.python3 # required for megahit (dependency should be upstreamed)

    pkgs.xz
    pkgs.zstd
    pkgs.which
  ];

}
