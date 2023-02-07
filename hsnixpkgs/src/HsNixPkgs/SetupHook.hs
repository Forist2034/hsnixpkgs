module HsNixPkgs.SetupHook (SetupHook (..)) where

import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import HsNixPkgs.ExtendDrv

data SetupHook m = SetupHook
  { newEnv :: HM.HashMap Text (DrvStr m),
    newPassAsFile :: HM.HashMap Text (DrvStr m)
  }

deriving instance (ApplicativeDeriv m) => Show (SetupHook m)

instance Default (SetupHook m) where
  def =
    SetupHook
      { newEnv = HM.empty,
        newPassAsFile = HM.empty
      }

instance (ApplicativeDeriv m) => Semigroup (SetupHook m) where
  l <> r =
    SetupHook
      { newEnv = newEnv l <> newEnv r,
        newPassAsFile = newPassAsFile l <> newPassAsFile r
      }

instance (ApplicativeDeriv m) => Monoid (SetupHook m) where
  mempty = def