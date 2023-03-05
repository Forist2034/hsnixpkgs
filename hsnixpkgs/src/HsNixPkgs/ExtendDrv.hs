module HsNixPkgs.ExtendDrv (
  DrvOutput (..),
  getStorePathStr,
  ExtendDeriv (..),
  getOutputAt,
  getDefaultOut,
  module FDrv,
) where

import Data.Foldable
import Data.Hashable
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import HsNix.Derivation
import HsNix.DrvStr (DrvStr)
import HsNix.OutputName
import HsNixPkgs.ExtendDrv.FileDrv as FDrv
import HsNixPkgs.System

data DrvOutput (h :: System) (t :: System) = DrvOutput
  { dOutParentDrv :: Derivation,
    dOutName :: Maybe OutputName
  }
  deriving (Show, Eq)

instance Hashable (DrvOutput h t) where
  hashWithSalt s d = hashWithSalt s (dOutParentDrv d, dOutName d)

getStorePathStr :: DrvOutput h t -> DrvM DrvStr
getStorePathStr d = drvStorePathStrOf (dOutParentDrv d) (dOutName d)

data ExtendDeriv h t = ExtDrv
  { eDrvBaseDrv :: Derivation,
    eDrvOutputs :: NEL.NonEmpty (DrvOutput h t)
  }
  deriving (Show)

instance Eq (ExtendDeriv h t) where
  l == r = eDrvBaseDrv l == eDrvBaseDrv r

getOutputAt :: ExtendDeriv h t -> Maybe OutputName -> DrvOutput h t
getOutputAt d on@(Just _) = fromJust (find (\i -> dOutName i == on) (eDrvOutputs d))
getOutputAt d Nothing = NEL.head (eDrvOutputs d)

getDefaultOut :: ExtendDeriv h t -> DrvOutput h t
getDefaultOut d = getOutputAt d Nothing
