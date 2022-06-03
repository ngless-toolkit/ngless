let
  nixpkgsPinned = builtins.fetchGit {
    name = "nixpksg-pinned";
    url = "https://github.com/nixos/nixpkgs/";
    rev = "1bb17332abf7a782ed7fa3101fd7ba54dd239694";
  };

  sources = {
    haskellNix = builtins.fetchTarball {
        name = "haskell-nix-snap";
        url = "https://github.com/input-output-hk/haskell.nix/archive/c6a5afba7e259e9908f9dc56ce711173cda4549b.tar.gz";
        sha256 = "0g58zvyqd47c1y2w84qbf1x29yp4pfdz2gqligvgn1jlx972i53n";
    };
  };

  haskellNix = import sources.haskellNix { };

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    nixpkgsPinned
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


in pkgs.stdenv.mkDerivation {
    name = "ngless-source-with-dependencies";
    version = "1.4.1";
    nativeBuildInputs = with pkgsStatic ; [ megahit-static samtools bwa prodigal minimap2 ];
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
        ln -s ${pkgsStatic.samtools}/bin/samtools ngless_samtools_static
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
