{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.StdEnv.Linux.GLibc.Amd64 (bootStdEnv) where

import qualified HsNix.System as NS
import HsNixPkgs.Dependent
import HsNixPkgs.Develop.Haskell.Package
import HsNixPkgs.ExtendDrv
import HsNixPkgs.SetupHook.Executable
import HsNixPkgs.StdEnv.BootTools.Derivation
import HsNixPkgs.StdEnv.BootTools.RequireFile
import HsNixPkgs.StdEnv.BootTools.TH
import HsNixPkgs.StdEnv.Linux.SupportTools.Amd64
import HsNixPkgs.StdEnv.StdEnv
import HsNixPkgs.System

decodeSpec "stdenv/linux/amd64/glibc.yaml"
  >>= mkBootDerivations
    [|unpackStdenvLinux|]
    NS.x86_64_linux
    [|requireFile|]

mkExec :: UnpackedDeriv m -> Executable b h t m
mkExec bd =
  Executable
    { execDrvOutput =
        DrvOutput
          { dOutParentDrv = btDerivation bd,
            dOutName = Nothing
          },
      execSubDir = ["bin"],
      execDep = mempty
    }

bootStdEnv ::
  ApplicativeDeriv m =>
  StdEnv
    'BootStage
    Amd64_unknown_linux_gnu
    Amd64_unknown_linux_gnu
    Amd64_unknown_linux_gnu
    m
bootStdEnv =
  StdEnv
    { stdEnvGhc =
        DrvOutput
          { dOutParentDrv = btDerivation ghc,
            dOutName = Nothing
          },
      compileBuilderBin =
        DrvOutput
          { dOutParentDrv = compileBuilder,
            dOutName = Nothing
          },
      builderLibBoot =
        HsPackage
          { hsOutputDrv =
              DrvOutput
                { dOutParentDrv = btDerivation builderlib_boot,
                  dOutName = Nothing
                },
            hsPkgs = ["hsnixpkgs-boot-builderlib"],
            hsDepends = mempty
          },
      stdEnvExec =
        mempty
          { depsBuildHost =
              [ mkExec coreutils,
                mkExec bash,
                mkExec findutils,
                mkExec diffutils,
                mkExec sed,
                mkExec grep,
                mkExec gawk,
                mkExec tar,
                mkExec gzip,
                mkExec bzip2,
                mkExec make,
                mkExec patch,
                mkExec busybox,
                mkExec patchelf,
                mkExec gcc,
                mkExec binutils
              ]
          }
    }
