{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.CompressManPages (compressManPages) where

import qualified HsNixPkgs.Boot.Build.Fixup.CompressManPages as B
import HsNixPkgs.Build.Main (BIO)
import Language.Haskell.GenPackage
import Language.Haskell.TH

compressManPages :: Code HsQ (FilePath -> BIO ())
compressManPages = [||B.compressManPages||]