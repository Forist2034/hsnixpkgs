{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module HsNixPkgs.ExtendDrv.FileDrv where

import Data.Hashable
import Data.Singletons.Base.TH
import Data.Text (Text)
import HsNix.Derivation

singletons
  [d|
    data FileDrvType
      = RegularFDrv
      | DirectoryFDrv
    |]

data FileDeriv m (t :: FileDrvType) = FileDrv
  { fDrvName :: Text,
    fDrvBase :: Derivation m,
    fDrvFilename :: Text
  }

deriving instance (ApplicativeDeriv m) => Show (FileDeriv m t)

deriving instance (ApplicativeDeriv m) => Eq (FileDeriv m t)

instance (ApplicativeDeriv m) => Hashable (FileDeriv m t) where
  hashWithSalt s v = hashWithSalt s (fDrvBase v)