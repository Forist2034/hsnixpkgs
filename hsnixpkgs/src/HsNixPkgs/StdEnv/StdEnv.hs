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

type StdEnvGhc = Ghc_9_0_2

data StdEnv (s :: StageSpec) (b :: System) (h :: System) (t :: System) m = StdEnv
  { ghc :: DrvOutput b b m,
    compileBuilderBin :: DrvOutput b b m,
    builderLibBoot :: HsPackage StdEnvGhc b b b m,
    stdEnvExec :: SimpleDeps [] Executable b h t m
  }