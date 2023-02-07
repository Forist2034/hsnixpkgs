module HsNixPkgs.ExtendDrv
  ( AsDrvStr (..),
    HasStrBuilder (..),
    ApplicativeDeriv (..),
    DrvOutput (..),
    getStorePathStr,
    ExtendDeriv (..),
    getOutputAt,
    getDefaultOut,
    module FDrv,
  )
where

import Data.Foldable
import Data.Hashable
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import HsNix.Derivation
import HsNixPkgs.ExtendDrv.FileDrv as FDrv
import HsNixPkgs.System

data DrvOutput (h :: System) (t :: System) m = DrvOutput
  { dOutParentDrv :: Derivation m,
    dOutName :: Maybe Text
  }

deriving instance (ApplicativeDeriv m) => Show (DrvOutput h t m)

instance (ApplicativeDeriv m) => Eq (DrvOutput h t m) where
  a == b =
    dOutParentDrv a == dOutParentDrv b
      && dOutName a == dOutName b

instance (ApplicativeDeriv m) => Hashable (DrvOutput h t m) where
  hashWithSalt s d = hashWithSalt s (dOutParentDrv d, dOutName d)

getStorePathStr :: ApplicativeDeriv m => DrvOutput h t m -> m (DrvStr m)
getStorePathStr d = storePathStrOf (dOutParentDrv d) (dOutName d)

data ExtendDeriv h t m = ExtDrv
  { eDrvBaseDrv :: Derivation m,
    eDrvOutputs :: NEL.NonEmpty (DrvOutput h t m)
  }

deriving instance (ApplicativeDeriv m) => Show (ExtendDeriv h t m)

deriving instance (ApplicativeDeriv m) => Eq (ExtendDeriv h t m)

getOutputAt :: ExtendDeriv m h t -> Maybe Text -> DrvOutput m h t
getOutputAt d on@(Just _) = fromJust (find (\i -> dOutName i == on) (eDrvOutputs d))
getOutputAt d Nothing = NEL.head (eDrvOutputs d)

getDefaultOut :: ExtendDeriv m h t -> DrvOutput m h t
getDefaultOut d = getOutputAt d Nothing
