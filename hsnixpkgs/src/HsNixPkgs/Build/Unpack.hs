{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Unpack
  ( UnpackP,
    UnpackCfg (..),
    SourceList,
    unpackExpr,
    mkUnpackPhase,
    unpackPhase,
  )
where

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
import HsNixPkgs.HsBuilder.Generate
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
                      Just (\s -> unsafeCodeCoerce [|B.unTarXz $(unTypeCode s)|])
                  | any (`T.isSuffixOf` t) [".tar", ".tgz", ".tbz2", ".tbz"] ->
                      Just (\s -> unsafeCodeCoerce [|B.unTar $(unTypeCode s)|])
                  | otherwise -> Nothing
          ],
        makeSourcesWritable = True,
        unpackHook = def
      }

type SourceList m = NEL.NonEmpty (Sigma FileDrvType (TyCon1 (FileDeriv m)))

unpackExpr ::
  forall mt m.
  ApplicativeDeriv m =>
  SourceList m ->
  String ->
  UnpackCfg ->
  DepModM mt m (Code HsQ (BIO ()))
unpackExpr sources sourceRoot cfg = do
  s <-
    useModule
      ( mkKVMod
          "Unpack.Sources"
          ["base"]
          []
          ( fmap
              ( \(_ :&: f) ->
                  ( T.unpack (fDrvName f),
                    "FilePath",
                    DepStr (ND.storePathStr (fDrvBase f))
                  )
              )
              sources
          )
      )
  pure
    ( runHook
        (unpackHook cfg)
        ( unsafeCodeCoerce
            [|
              B.unpackFunc
                $( listE
                     ( NEL.toList
                         ( NEL.zipWith
                             ( \u@(_ :&: f) e ->
                                 tupE
                                   [ lift (T.unpack (fDrvName f)),
                                     selectUnpack u e
                                   ]
                             )
                             sources
                             s
                         )
                     )
                 )
                $(lift sourceRoot)
                $(lift (makeSourcesWritable cfg))
              |]
        )
    )
  where
    selectUnpack ::
      Sigma FileDrvType (TyCon1 (FileDeriv m)) ->
      Name ->
      HsQ Exp
    selectUnpack (SDirectoryFDrv :&: _) e = [|copyFile $(varE e) "."|]
    selectUnpack (SRegularFDrv :&: f) e =
      unTypeCode
        ( head
            ( mapMaybe
                (\uf -> uf (fDrvFilename f))
                (unpackCmds cfg)
            )
            (unsafeCodeCoerce (varE e))
        )

mkUnpackPhase :: Code HsQ (BIO ()) -> HsQ [Dec] -> ModuleM s mt (Code HsQ Phase)
mkUnpackPhase = newPhase "unpackPhrase" "unpacking sources"

unpackPhase ::
  ApplicativeDeriv m =>
  SourceList m ->
  String ->
  UnpackCfg ->
  DepModM mt m (Code HsQ Phase)
unpackPhase s sr cfg = do
  fun <- unpackExpr s sr cfg
  mkUnpackPhase fun (pure [])
