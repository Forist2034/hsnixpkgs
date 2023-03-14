{ pkgs ? (import <nixpkgs> {
  overlays = [
    (final: prev: {
      haskellPackages = prev.haskell.packages.ghc943;
      libffi = prev.libffi.overrideAttrs (old: { dontDisableStatic = true; });
      gmp6 = prev.gmp6.override { withStatic = true; };
    })
  ];
}).pkgsMusl }:
with pkgs;
let
  hsPkgStatic = drv:
    haskell.lib.appendConfigureFlags
      drv
      [ "--enable-executable-static"
        "--extra-lib-dirs=${zlib.static}/lib"
      ];
in rec {
  coreutilsMinimal = coreutils.override (args: {
    # We want coreutils without ACL/attr support.
    aclSupport = false;
    attrSupport = false;
    # Our tooling currently can't handle scripts in bin/, only ELFs and symlinks.
    singleBinary = "symlinks";
  });

  tarMinimal = gnutar.override { acl = null; };

  busyboxMinimal = busybox.override {
    useMusl = !stdenv.targetPlatform.isRiscV;
    enableStatic = true;
    enableMinimal = true;
    extraConfig = ''
      CONFIG_ASH y
      CONFIG_ASH_ECHO y
      CONFIG_ASH_TEST y
      CONFIG_ASH_OPTIMIZE_FOR_SIZE y
      CONFIG_MKDIR y
      CONFIG_TAR y
      CONFIG_UNXZ y
    '';
  };

  bootGCC = gcc.cc.override { enableLTO = false; };
  bootBinutils = binutils.bintools.override {
    withAllTargets = false;
    # Don't need two linkers, disable whatever's not primary/default.
    enableGold = false;
    # bootstrap is easier w/static
    enableShared = false;
  };

  builderlib-boot = haskell.lib.doJailbreak (haskellPackages.callPackage ../../builderlib/boot {});

  compile-builder = hsPkgStatic (haskellPackages.callPackage ../../compile-builder {});

  xz = pkgs.xz.override { enableStatic = true; };

  hsPackages = with haskell.lib;
    let
      dep = import ./dependencies.nix;
    in haskellPackages.override {
        overrides = final: prev: {
          unix = dontCheck prev.unix_2_8_0_0;
          filepath = dontCheck prev.filepath_1_4_100_0;
          directory = dontCheck prev.directory_1_3_8_0;
          process = prev.process_1_6_16_0;
          hashable = final.callPackage dep.hashable {};
          hspec-core = dontCheck (final.callPackage dep.hspec-core {});
          hspec-discover = dontCheck (final.callPackage dep.hspec-discover {});
          hspec = final.callPackage dep.hspec {};
          foldl = dontCheck prev.foldl;

          nix-archive = final.callPackage ./nix-archive.nix {};
          stdenv-unpack = 
            haskell.lib.appendConfigureFlags
              ( hsPkgStatic (final.callPackage ../unpack {})) 
              [ "--extra-lib-dirs=${xz.out}/lib" ];
          stdenv-pack = final.callPackage ../pack {};
        };
      };

  build =
    let
      packages = [
          [ "coreutils" coreutilsMinimal.out]
          [ "bash" bash.out]
          [ "findutils" findutils.out]
          [ "diffutils" diffutils.out]
          [ "sed" gnused.out]
          [ "grep" gnugrep.out]
          [ "gawk" gawk.out]
          [ "tar" tarMinimal.out]
          [ "gzip" gzip.out]
          [ "bzip2" bzip2.bin]
          [ "make" gnumake.out]
          [ "patch" patch]
          [ "patchelf" patchelf]
          [ "gcc" bootGCC.out]
          [ "gcc-lib" bootGCC.lib]
          [ "ghc" haskell.compiler.ghc943.out]
          [ "builderlib-boot" builderlib-boot.out]
          [ "binutils" bootBinutils.out]
          [ "busybox" busyboxMinimal]
        ];
   in
    stdenv.mkDerivation {
      name = "stdenv-bootstrap-tools";

      exportReferencesGraph = builtins.concatLists packages;
      
      graphs = map builtins.head packages;

      exported = builtins.concatStringsSep "\n" (map (p: builtins.toString (builtins.elemAt p 1) + " " + builtins.head p) packages);

      passAsFile = [ "exported" ];

      buildCommand = ''
        ${hsPackages.stdenv-pack.out}/bin/pack-stdenv-linux graphs exportedPath ${compile-builder} ${hsPackages.stdenv-unpack} +RTS -maxN$NIX_BUILD_CORES
        '';
    };
}
