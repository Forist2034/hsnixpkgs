{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Unpack (
  UnpackP,
  UnpackCfg (..),
  SourceList,
  unpackExpr,
  mkUnpackPhase,
  unpackPhase,
) where

import Data.Default
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Singletons
import Data.Singletons.Sigma
import Data.Text (Text)
import qualified Data.Text as T
import qualified HsNix.Derivation as ND
import qualified HsNixPkgs.Boot.Build.Unpack as B
import HsNixPkgs.Build.Hook
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.Build.Phase
import HsNixPkgs.ExtendDrv
import HsNixPkgs.HsBuilder.DepStr
import HsNixPkgs.HsBuilder.KVMod
import HsNixPkgs.HsBuilder.Util
import Language.Haskell.GenPackage
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory

data UnpackP

data UnpackCfg = UnpackCfg
  { unpackCmds ::
      [ Text ->
        Maybe
          ( Code HsQ FilePath ->
            Code HsQ (IO ())
          )
      ],
    makeSourcesWritable :: Bool,
    unpackHook :: Hook UnpackP
  }

instance Default UnpackCfg where
  def =
    UnpackCfg
      { unpackCmds =
          [ \t ->
              if
                  | any (`T.isSuffixOf` t) [".tar.xz", ".tar.lzma", ".txz"] ->
                      Just (\s -> [||B.unTarXz $$s||])
                  | any (`T.isSuffixOf` t) [".tar", ".tgz", ".tbz2", ".tbz"] ->
                      Just (\s -> [||B.unTar $$s||])
                  | otherwise -> Nothing
          ],
        makeSourcesWritable = True,
        unpackHook = def
      }

type SourceList = NEL.NonEmpty (Sigma FileSrcType (TyCon1 FileSource))

unpackExpr ::
  forall mt.
  SourceList ->
  String ->
  UnpackCfg ->
  ModuleM mt (Code HsQ (BIO ()))
unpackExpr sources sourceRoot cfg = do
  s <-
    useModule
      ( mkKVMod
          "Unpack.Sources"
          ["base"]
          []
          ( fmap
              ( \(_ :&: f) ->
                  KVPair
                    { kvKey = T.unpack (fSrcName f),
                      kvType = "FilePath",
                      kvValue = DepStr (ND.srcStorePathStr (fSrcBase f))
                    }
              )
              sources
          )
      )
  pure
    ( runHook
        (unpackHook cfg)
        [||
        B.unpackFunc
          $$( listET
                ( NEL.toList
                    ( NEL.zipWith
                        ( \u@(_ :&: f) e ->
                            [||
                            ( $$(liftTyped (T.unpack (fSrcName f))),
                              $$(selectUnpack u (unsafeCodeCoerce (varE e)))
                            )
                            ||]
                        )
                        sources
                        s
                    )
                )
            )
          $$(liftTyped sourceRoot)
          $$(liftTyped (makeSourcesWritable cfg))
        ||]
    )
  where
    selectUnpack ::
      Sigma FileSrcType (TyCon1 FileSource) ->
      Code HsQ FilePath ->
      Code HsQ (IO ())
    selectUnpack (SDirectoryFSrc :&: _) e = [||copyFile $$e "."||]
    selectUnpack (SRegularFSrc :&: f) e =
      head
        ( mapMaybe
            (\uf -> uf (fSrcName f))
            (unpackCmds cfg)
        )
        e

mkUnpackPhase :: Code HsQ (BIO ()) -> HsQ [Dec] -> ModuleM mt (Code HsQ Phase)
mkUnpackPhase = newPhase "unpackPhrase" "unpacking sources"

unpackPhase ::
  SourceList ->
  String ->
  UnpackCfg ->
  ModuleM mt (Code HsQ Phase)
unpackPhase s sr cfg = do
  fun <- unpackExpr s sr cfg
  mkUnpackPhase fun (pure [])
