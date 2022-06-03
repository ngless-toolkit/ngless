{ version ? "latest" }:
let
  release = import ../release.nix;
  pkgs = release.pkgs;

in pkgs.dockerTools.buildImage {

  name = "nglesstoolkit/ngless";
  tag = version;

  contents = [
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

  config = {
      Env = [
        "NGLESS_SAMTOOLS_BIN=/bin/samtools"
        "NGLESS_BWA_BIN=/bin/bwa"
        "NGLESS_PRODIGAL_BIN=/bin/prodigal"
        "NGLESS_MEGAHIT_BIN=/bin/megahit"
        "NGLESS_MINIMAP2_BIN=/bin/minimap2"
      ];
    };


}
