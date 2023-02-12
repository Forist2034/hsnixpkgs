module HsNixPkgs.HsBuilder.Util (listET) where

import Language.Haskell.TH

listET :: Quote m => [Code m a] -> Code m [a]
listET = unsafeCodeCoerce . listE . fmap unTypeCode