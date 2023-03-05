{-# LANGUAGE DeriveLift #-}

module HsNixPkgs.StdEnv.BootTools.UnpackArg (UnpackDerivArg (..)) where

import Data.Text (Text)
import HsNix.System (System)
import Language.Haskell.TH.Syntax (Lift)

data UnpackDerivArg = UnpackDerivArg
  { udaName :: Text,
    udaSystem :: System,
    udaStoreName :: Text,
    udaOrigStorePath :: Text
  }
  deriving (Show, Lift)