let
 release = (import ./ngless-static-embed-dependencies.nix) {};
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

  shellHook = ''
    export NGLESS_SAMTOOLS_BIN=${pkgs.samtools}/bin/samtools
    export NGLESS_BWA_BIN=${pkgs.bwa}/bin/bwa
    export NGLESS_PRODIGAL_BIN=${pkgs.prodigal}/bin/prodigal
    export NGLESS_MEGAHIT_BIN=${pkgs.megahit}/bin/megahit
    export NGLESS_MINIMAP2_BIN=${pkgs.minimap2}/bin/minimap2
  '';

}
