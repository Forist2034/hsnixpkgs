{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-partial-fields #-}

module HsNixPkgs.Develop.Haskell.Package where

import Data.Functor
import Data.Hashable
import Data.Singletons.Base.TH
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits
import HsNix.Derivation
import HsNix.DrvStr (DrvStr)
import HsNixPkgs.Dependent
import HsNixPkgs.Develop.C.Library
import HsNixPkgs.ExtendDrv

singletons
  [d|
    data GHCVersion = GHCVersion Natural Natural Natural
      deriving (Eq, Ord, Show)

    newtype HSCompiler = HscGHC GHCVersion
      deriving (Eq, Ord, Show)

    ghc_9_4_3 :: HSCompiler
    ghc_9_4_3 = HscGHC (GHCVersion 9 4 3)
    |]

data HsDependent (hsc :: HSCompiler) b h t
  = HsPackageDep (HsPackage hsc b h t)
  | HsFFIDep (CLibrary b h t)
  deriving (Eq, Show, Generic)

instance Hashable (HsDependent hsc b h t)

instance HasPropagatedDep (HsDependent hsc) [] where
  propagatedDep (HsPackageDep p) = hsDepends p
  propagatedDep (HsFFIDep _) = mempty

data HsPackage (hsc :: HSCompiler) b h t = HsPackage
  { hsOutputDrv :: DrvOutput h t,
    hsPkgs :: [Text],
    hsDepends :: SimpleDeps [] (HsDependent hsc) b h t
  }
  deriving (Show)

instance Eq (HsPackage hsc b h t) where
  l == r = hsOutputDrv l == hsOutputDrv r

instance Hashable (HsPackage hsc b h t) where
  hashWithSalt s a = hashWithSalt s (hsOutputDrv a)

packageConfDir ::
  forall hsc b h t.
  SingI hsc =>
  HsPackage hsc b h t ->
  DrvM DrvStr
packageConfDir pkg =
  let (HscGHC (GHCVersion ma mi r)) = fromSing (sing @hsc)
   in getStorePathStr (hsOutputDrv pkg) <&> \o ->
        mconcat
          [ o,
            "/lib/ghc-",
            fromString (show ma),
            ".",
            fromString (show mi),
            ".",
            fromString (show r),
            "/package.conf.d"
          ]