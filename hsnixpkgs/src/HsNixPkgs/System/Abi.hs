{-# LANGUAGE DerivingStrategies #-}
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

module HsNixPkgs.System.Abi where

import Data.Singletons.Base.TH
import Data.Text (Text)

singletons
  [d|
    data Abi
      = GnuAbi
      | MuslAbi
      | UnknownAbi
      deriving (Eq, Show)
    |]

abiName :: Abi -> Text
abiName GnuAbi = "gnu"
abiName MuslAbi = "musl"
abiName UnknownAbi = "unknown"