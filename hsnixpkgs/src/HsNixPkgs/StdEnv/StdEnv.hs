{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module HsNixPkgs.StdEnv.StdEnv where

import Data.Singletons.Base.TH
import HsNixPkgs.Dependent
import HsNixPkgs.Develop.Haskell.Package
import HsNixPkgs.ExtendDrv
import HsNixPkgs.SetupHook.Executable
import HsNixPkgs.System

singletons
  [d|
    data StageSpec
      = BootStage
      | RegularStage
    |]

type StdEnvGhc = Ghc_9_4_3

data StdEnv (s :: StageSpec) (b :: System) (h :: System) (t :: System) = StdEnv
  { stdEnvGhc :: DrvOutput b b,
    compileBuilderBin :: DrvOutput b b,
    builderLibBoot :: HsPackage StdEnvGhc b b b,
    stdEnvExec :: SimpleDeps [] Executable b h t
  }