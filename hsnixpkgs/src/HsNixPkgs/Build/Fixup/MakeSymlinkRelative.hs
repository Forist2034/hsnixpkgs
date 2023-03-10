{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.MakeSymlinkRelative (makeSymlinkRelative) where

import qualified HsNixPkgs.Boot.Build.Fixup.MakeSymlinkRelative as B
import HsNixPkgs.Build.Main (BIO)
import Language.Haskell.GenPackage
import Language.Haskell.TH

makeSymlinkRelative :: Code HsQ (FilePath -> BIO ())
makeSymlinkRelative = [||B.makeSymlinkRelative||]