module HsNixPkgs.SetupHook (SetupHook (..)) where

import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import HsNix.DrvStr (DrvStr)

data SetupHook = SetupHook
  { newEnv :: HM.HashMap Text DrvStr,
    newPassAsFile :: HM.HashMap Text DrvStr
  }
  deriving (Show)

instance Default SetupHook where
  def =
    SetupHook
      { newEnv = HM.empty,
        newPassAsFile = HM.empty
      }

instance Semigroup SetupHook where
  l <> r =
    SetupHook
      { newEnv = newEnv l <> newEnv r,
        newPassAsFile = newPassAsFile l <> newPassAsFile r
      }

instance Monoid SetupHook where
  mempty = def