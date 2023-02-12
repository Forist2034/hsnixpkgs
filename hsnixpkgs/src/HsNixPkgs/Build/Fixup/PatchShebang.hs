{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.PatchShebang (patchSheBang) where

import qualified HsNixPkgs.Boot.Build.Fixup.PatchShebang as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH

patchSheBang :: Code HsQ [FilePath] -> Code HsQ (FilePath -> BIO ())
patchSheBang path = [||B.patchSheBang $$path||]