{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.MultiOutput (multiOutputFixup) where

import qualified HsNixPkgs.Boot.Build.Fixup.MultipleOutput as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.Build.MultiOutput
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH

multiOutputFixup ::
  MultiOutput (Code HsQ FilePath) ->
  Code HsQ (FilePath -> BIO ())
multiOutputFixup m =
  [||
  B.multiOutputFixup
    B.MultiOutput
      { B.outDev = $$(outDev m),
        B.outBin = $$(outBin m),
        B.outInclude = $$(outInclude m),
        B.outLib = $$(outLib m),
        B.outDoc = $$(outDoc m),
        B.outDevDoc = $$(outDevDoc m),
        B.outMan = $$(outMan m),
        B.outDevMan = $$(outDevMan m),
        B.outInfo = $$(outInfo m)
      }
  ||]
