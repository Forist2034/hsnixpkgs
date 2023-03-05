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
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-redundant-constraints #-}

module HsNixPkgs.System.Vendor where

import Data.Singletons.Base.TH
import Data.Text (Text)

singletons
  [d|
    data Vendor
      = Pc
      | None
      | UnknownVendor
      deriving (Eq, Show)
    |]

vendorName :: Vendor -> Text
vendorName Pc = "pc"
vendorName None = "none"
vendorName UnknownVendor = "unknown"