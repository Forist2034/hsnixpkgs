{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module HsNixPkgs.System.Kernel where

import Data.Singletons.Base.TH
import Data.Text (Text)

singletons
  [d|
    data ExecFormat
      = Elf
      | Aout
      deriving (Eq, Show)

    data KernelFamily = Bsd
      deriving (Eq, Show)

    data Kernel = Linux
      deriving (Eq, Show)

    kExecFormat :: Kernel -> ExecFormat
    kExecFormat Linux = Elf
    |]

kernelName :: Kernel -> Text
kernelName Linux = "linux"