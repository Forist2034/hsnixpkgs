{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-redundant-constraints #-}

module HsNixPkgs.System.System where

import Data.Singletons.Base.TH
import Data.Text (Text)
import qualified HsNix.System as NS
import HsNixPkgs.System.Abi
import HsNixPkgs.System.Cpu
import HsNixPkgs.System.Kernel
import HsNixPkgs.System.Vendor

singletons
  [d|
    data System = System Cpu Vendor Kernel Abi
      deriving (Eq, Show)

    amd64_unknown_linux_gnu :: System
    amd64_unknown_linux_gnu = System amd64 UnknownVendor Linux GnuAbi
    |]

canExecute :: System -> System -> Bool
canExecute (System ec _ ek _) (System hc _ hk _) = isCompatible ec hc && ek == hk

isLinux :: System -> Bool
isLinux (System _ _ Linux _) = True

isStatic :: System -> Bool
isStatic = const False

toNixSys :: System -> NS.System
toNixSys sys@(System cpu _ kernel _) =
  if cpu == amd64 && kernel == Linux
    then NS.x86_64_linux
    else error ("Unsupported system " ++ show sys)

toConfigTriple :: System -> Text
toConfigTriple (System cpu vendor kernel abi) =
  mconcat
    [ cpuName cpu,
      "-",
      vendorName vendor,
      "-",
      kernelName kernel,
      if abi /= UnknownAbi then "-" <> abiName abi else mempty
    ]

toEnvSuffix :: System -> Text
toEnvSuffix (System cpu vendor kernel abi) =
  mconcat
    [ cpuName cpu,
      "_",
      vendorName vendor,
      "_",
      kernelName kernel,
      if abi /= UnknownAbi then "_" <> abiName abi else mempty
    ]