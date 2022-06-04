let
 release = (import ./release.nix) {};
 pkgs = release.pkgs;

in pkgs.mkShell {

  name = "nglessEnv";

  nativeBuildInputs = [
    release.NGLess.components.exes.ngless
    pkgs.prodigal
    pkgs.samtools
    pkgs.megahit
    pkgs.python3 # required for megahit (dependency should be upstreamed)
    pkgs.minimap2
    pkgs.bwa

    pkgs.xz
    pkgs.which
    pkgs.zstd
  ];
  shellHook = ''
    export NGLESS_SAMTOOLS_BIN=${pkgs.samtools}/bin/samtools
    export NGLESS_BWA_BIN=${pkgs.bwa}/bin/bwa
    export NGLESS_PRODIGAL_BIN=${pkgs.prodigal}/bin/prodigal
    export NGLESS_MEGAHIT_BIN=${pkgs.megahit}/bin/megahit
    export NGLESS_MINIMAP2_BIN=${pkgs.minimap2}/bin/minimap2
  '';

}
