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
  unsafeCodeCoerce
    [|
      B.multiOutputFixup
        B.MultiOutput
          { B.outDev = $(unTypeCode (outDev m)),
            B.outBin = $(unTypeCode (outBin m)),
            B.outInclude = $(unTypeCode (outInclude m)),
            B.outLib = $(unTypeCode (outLib m)),
            B.outDoc = $(unTypeCode (outDoc m)),
            B.outDevDoc = $(unTypeCode (outDevDoc m)),
            B.outMan = $(unTypeCode (outMan m)),
            B.outDevMan = $(unTypeCode (outDevMan m)),
            B.outInfo = $(unTypeCode (outInfo m))
          }
      |]
