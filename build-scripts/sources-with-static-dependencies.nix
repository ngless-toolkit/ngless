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
    haskellNix.sources.nixpkgs-unstable
    haskellNix.nixpkgsArgs;

  ignoredPaths = ["tests" "docs" "run-tests.sh"];

  pkgsStatic = pkgs.pkgsCross.musl64.pkgsStatic;

  # Ideally, we would love to use pkgsStatic.megahit
  # However, that crashes because of some openmp issue with musl and static
  # linking
  megahit-static = (pkgs.megahit.overrideAttrs (oldAttr: {
        cmakeFlags = [ "-DSTATIC_BUILD=ON" ];
        nativeBuildInputs = oldAttr.nativeBuildInputs ++ [ pkgs.zlib.dev pkgs.glibc.static ];
  }) ).override { zlib = pkgs.zlib.static; };

  samtools-static = (pkgsStatic.samtools.overrideAttrs (oldAttr: {
        doCheck = false;
  }) );


in pkgs.stdenv.mkDerivation {
    name = "ngless-source-with-dependencies";
    version = "1.5.0";
    nativeBuildInputs = with pkgsStatic ; [ megahit-static samtools-static bwa prodigal minimap2 ];
    buildInputs = with pkgs; [ xxd stdenv ];
    src = pkgs.lib.cleanSourceWith {
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "NGLess";
        src = ../.;
      };
      # ignore paths that change frequently, but do not contribute to the result
      filter = path: type: let baseName = baseNameOf (toString path); in !(pkgs.lib.elem baseName ignoredPaths);
    };
    configurePhase = "";
    buildPhase = ''
        patch -p1 < build-scripts/set-embed.patch
    '';

    installPhase = ''
        mkdir -p $out
        cp -pir NGLess $out
        cp -pir Execs $out
        cp -pir Html $out
        cp -pir Tests-Src $out
        mkdir -p $out/NGLess/Dependencies/

        # xxd bases its output on the input *filename*
        ln -s ${samtools-static}/bin/samtools ngless_samtools_static
        xxd -i ngless_samtools_static > $out/NGLess/Dependencies/samtools_data.c

        ln -s ${pkgsStatic.prodigal}/bin/prodigal ngless_prodigal_static
        xxd -i ngless_prodigal_static > $out/NGLess/Dependencies/prodigal_data.c

        ln -s ${pkgsStatic.bwa}/bin/bwa ngless_bwa_static
        xxd -i ngless_bwa_static > $out/NGLess/Dependencies/bwa_data.c

        ln -s ${pkgsStatic.minimap2}/bin/minimap2 ngless_minimap2_static
        xxd -i ngless_minimap2_static > $out/NGLess/Dependencies/minimap2_data.c

        mkdir megahit_packaged
        cp -pir ${megahit-static}/bin/megahit_core \
                    ${megahit-static}/bin/megahit_core_popcnt \
                    ${megahit-static}/bin/megahit_core_no_hw_accel \
                    ${megahit-static}/bin/megahit_toolkit \
                    ${megahit-static}/bin/megahit \
                    megahit_packaged
        tar czf megahit_packaged.tar.gz megahit_packaged
        xxd -i megahit_packaged.tar.gz > $out/NGLess/Dependencies/megahit_data.c

        cp -pir stack.yaml $out/
        cp -pir package.yaml $out/
        cp -pir COPYING $out/
        cp -pir README.md $out/
    '';
}
