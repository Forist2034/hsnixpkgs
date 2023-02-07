{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup
  ( FixupP,
    FixupTarget (..),
    fixupExpr,
    mkFixupPhase,
    fixupPhase,
  )
where

import qualified HsNixPkgs.Boot.Build.Fixup as B
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Build.Hook
import HsNixPkgs.Build.Phase
import HsNixPkgs.HsBuilder.DepStr
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH

data FixupP

data FixupTarget = FixupTarget
  { ftPath :: Code HsQ FilePath,
    ftFixupFunc :: [Code HsQ (FilePath -> BIO ())]
  }

fixupExpr ::
  Hook FixupP ->
  [FixupTarget] ->
  Code HsQ (BIO ())
fixupExpr h fs =
  unsafeCodeCoerce
    [|
      B.makeWritable $(listE (unTypeCode . ftPath <$> fs))
        >> $( unTypeCode
                ( runHook
                    h
                    (unsafeCodeCoerce (doE (fmap (noBindS . runFixup) fs)))
                )
            )
      |]
  where
    runFixup t =
      [|
        B.runFixup
          $(unTypeCode (ftPath t))
          $(listE (unTypeCode <$> ftFixupFunc t))
        |]

mkFixupPhase :: Code HsQ (BIO ()) -> HsQ [Dec] -> ModuleM s mt (Code HsQ Phase)
mkFixupPhase = newPhase "unpackPhase" "unpacking sources"

fixupPhase :: Hook FixupP -> [FixupTarget] -> DepModM mt m (Code HsQ Phase)
fixupPhase h fs = mkFixupPhase (fixupExpr h fs) (pure [])