module HsNixPkgs.HsBuilder.DepStr (
  DepStr (..),
) where

import Control.Applicative
import Data.String
import HsNix.Derivation
import HsNix.DrvStr (DrvStr)

newtype DepStr = DepStr {getDepStr :: DrvM DrvStr}

instance IsString DepStr where
  fromString = DepStr . pure . fromString

instance Semigroup DepStr where
  DepStr l <> DepStr r = DepStr (liftA2 (<>) l r)

instance Monoid DepStr where
  mempty = DepStr (pure mempty)
