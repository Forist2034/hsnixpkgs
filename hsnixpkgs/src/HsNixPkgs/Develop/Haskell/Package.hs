{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-partial-fields #-}

module HsNixPkgs.Develop.Haskell.Package where

import Data.Functor
import Data.Hashable
import Data.Singletons.Base.TH
import Data.Singletons.TH.Options
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits
import HsNixPkgs.Dependent
import HsNixPkgs.Develop.C.Library
import HsNixPkgs.ExtendDrv
import Numeric.Natural

data GHCVersion = GHCVersion Natural Natural Natural
  deriving (Eq, Ord, Show)

data PGHCVersion = PGHCVersion Nat Nat Nat

newtype HSCompiler = HscGHC GHCVersion
  deriving (Eq, Ord, Show)

newtype PHSCompiler = PHscGHC PGHCVersion

let customPromote n
      | n == ''GHCVersion = ''PGHCVersion
      | n == 'GHCVersion = 'PGHCVersion
      | n == ''HSCompiler = ''PHSCompiler
      | n == 'HscGHC = 'PHscGHC
      | n == ''Natural = ''Nat
      | otherwise = promotedDataTypeOrConName defaultOptions n
 in withOptions
      defaultOptions
        { promotedDataTypeOrConName = customPromote,
          defunctionalizedName = defunctionalizedName defaultOptions . customPromote
        }
      $ concat
        <$> sequence
          [ genSingletons [''GHCVersion, ''HSCompiler],
            promoteEqInstances [''GHCVersion, ''HSCompiler],
            singletons
              [d|
                ghc_9_0_2 :: HSCompiler
                ghc_9_0_2 = HscGHC (GHCVersion 9 0 2)
                |]
          ]

data HsDependent (hsc :: PHSCompiler) b h t m
  = HsPackageDep (HsPackage hsc b h t m)
  | HsFFIDep (CLibrary b h t m)
  deriving (Generic)

deriving instance (ApplicativeDeriv m) => Show (HsDependent hsc b h t m)

deriving instance (ApplicativeDeriv m) => Eq (HsDependent hsc b h t m)

instance (ApplicativeDeriv m) => Hashable (HsDependent hsc b h t m)

instance HasPropagatedDep (HsDependent hsc) [] where
  propagatedDep (HsPackageDep p) = hsDepends p
  propagatedDep (HsFFIDep _) = mempty

data HsPackage (hsc :: PHSCompiler) b h t m = HsPackage
  { hsOutputDrv :: DrvOutput h t m,
    hsPkgs :: [Text],
    hsDepends :: SimpleDeps [] (HsDependent hsc) b h t m
  }

deriving instance (ApplicativeDeriv m) => Show (HsPackage hsc b h t m)

deriving instance (ApplicativeDeriv m) => Eq (HsPackage hsc b h t m)

instance (ApplicativeDeriv m) => Hashable (HsPackage hsc b h t m) where
  hashWithSalt s a = hashWithSalt s (hsOutputDrv a)

packageConfDir ::
  forall hsc b h t m.
  (SingI hsc, ApplicativeDeriv m) =>
  HsPackage hsc b h t m ->
  m (DrvStr m)
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