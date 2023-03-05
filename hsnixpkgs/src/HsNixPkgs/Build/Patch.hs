{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module HsNixPkgs.Build.Patch (
  PatchP,
  PatchCfg (..),
  Patch (..),
  patchExpr,
  mkPatchPhase,
  patchPhase,
) where

import Control.Monad.IO.Class
import Data.Default
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified HsNix.Derivation as ND
import qualified HsNixPkgs.Boot.Build.Patch as B
import HsNixPkgs.Build.Hook
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.Build.Phase
import HsNixPkgs.ExtendDrv
import HsNixPkgs.HsBuilder.DepStr
import HsNixPkgs.HsBuilder.KVMod
import Language.Haskell.GenPackage
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data PatchP

data PatchCfg = PatchCfg
  { patchFlags :: [Code HsQ String],
    patchHook :: Hook PatchP
  }

instance Default PatchCfg where
  def =
    PatchCfg
      { patchFlags = [liftTyped "-p1"],
        patchHook = def
      }

newtype Patch = SrcPatch (FileSource 'RegularFSrc)

patchExpr ::
  NEL.NonEmpty Patch ->
  PatchCfg ->
  ModuleM mt (Code HsQ (BIO ()))
patchExpr patches cfg = do
  mp <-
    useModule
      ( mkKVMod
          "Patch.Sources"
          ["base"]
          []
          ( fmap
              ( \(SrcPatch sp) ->
                  KVPair
                    { kvKey = T.unpack (fSrcName sp),
                      kvType = "FilePath",
                      kvValue = DepStr (ND.srcStorePathStr (fSrcBase sp))
                    }
              )
              patches
          )
      )
  pure
    ( runHook
        (patchHook cfg)
        ( unsafeCodeCoerce
            ( do
                flagN <- newName "flags"
                let flagE = varE flagN
                [|liftIO|]
                  `appE` doE
                    ( fmap
                        LetS
                        [d|$(varP flagN) = $(listE (unTypeCode <$> patchFlags cfg))|]
                        : NEL.toList
                          ( NEL.zipWith
                              ( \p n ->
                                  noBindS (applyPatch p (varE n) flagE)
                              )
                              patches
                              mp
                          )
                    )
            )
        )
    )
  where
    applyPatch (SrcPatch f) e flag =
      [|
        B.applyPatch
          $(lift (T.unpack (fSrcName f)))
          $flag
          $( let fn = fSrcName f
              in if
                    | ".gz" `T.isSuffixOf` fn -> [|B.unGzip $e|]
                    | ".bz2" `T.isSuffixOf` fn -> [|B.unBzip2 $e|]
                    | ".xz" `T.isSuffixOf` fn -> [|B.unXz $e|]
                    | ".lzma" `T.isSuffixOf` fn -> [|B.unLzma $e|]
                    | otherwise -> error ("Unknown patch format " ++ show fn)
           )
        |]

mkPatchPhase ::
  Code HsQ (BIO ()) ->
  HsQ [Dec] ->
  ModuleM mt (Code HsQ Phase)
mkPatchPhase = newPhase "patchPhase" "patching sources"

patchPhase ::
  NEL.NonEmpty Patch ->
  PatchCfg ->
  ModuleM mt (Code HsQ Phase)
patchPhase p cfg = do
  fun <- patchExpr p cfg
  mkPatchPhase fun (pure [])