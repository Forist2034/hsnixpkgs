{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup (
  FixupP,
  FixupTarget (..),
  fixupExpr,
  mkFixupPhase,
  fixupPhase,
) where

import qualified HsNixPkgs.Boot.Build.Fixup as B
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Build.Hook
import HsNixPkgs.Build.Phase
import HsNixPkgs.HsBuilder.Util
import Language.Haskell.GenPackage
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
  [||
  B.makeWritable $$(listET (ftPath <$> fs))
    >> $$( runHook
            h
            (unsafeCodeCoerce (doE (fmap (noBindS . unTypeCode . runFixup) fs)))
         )
  ||]
  where
    runFixup t =
      [||
      B.runFixup
        $$(ftPath t)
        $$(listET (ftFixupFunc t))
      ||]

mkFixupPhase :: Code HsQ (BIO ()) -> HsQ [Dec] -> ModuleM mt (Code HsQ Phase)
mkFixupPhase = newPhase "unpackPhase" "unpacking sources"

fixupPhase :: Hook FixupP -> [FixupTarget] -> ModuleM mt (Code HsQ Phase)
fixupPhase h fs = mkFixupPhase (fixupExpr h fs) (pure [])