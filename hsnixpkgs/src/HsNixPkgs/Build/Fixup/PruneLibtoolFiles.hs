{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.PruneLibtoolFiles (pruneLibtoolFiles) where

import qualified HsNixPkgs.Boot.Build.Fixup.PruneLibtoolFiles as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH

pruneLibtoolFiles :: Code HsQ (FilePath -> BIO ())
pruneLibtoolFiles = [||B.pruneLibtoolFiles||]