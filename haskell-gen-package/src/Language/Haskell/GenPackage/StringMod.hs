module Language.Haskell.GenPackage.StringMod (
  StringModule (..),
  stringModule,
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Language.Haskell.GenPackage.Str
import Language.Haskell.GenPackage.Types

data StringModule a = StringModule
  { smText :: Str,
    smExtDep :: [String],
    smValue :: a
  }

stringModule :: String -> StringModule a -> HsModule 'OtherModule a
stringModule name sm =
  HSModule
    { modDepM =
        addDep
          name
          (HM.singleton name (smText sm), HS.fromList (smExtDep sm))
          (pure ()),
      modVal = smValue sm
    }