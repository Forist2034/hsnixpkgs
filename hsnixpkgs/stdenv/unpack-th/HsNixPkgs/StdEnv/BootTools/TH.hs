{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.StdEnv.BootTools.TH (
  decodeSpec,
  mkSupportTools,
  mkBootDerivations,
) where

import Data.Char
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified HsNix.Hash as NH
import HsNix.StorePathName
import HsNix.System
import HsNixPkgs.BootTools.Derivation
import HsNixPkgs.BootTools.SupportTools
import HsNixPkgs.StdEnv.BootTools.UnpackArg
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

fetchExp ::
  CodeQ (System -> StorePathName -> Bool -> NH.Hash NH.SHA256 -> a) ->
  System ->
  Text ->
  Bool ->
  Hash ->
  CodeQ a
fetchExp fetcher sys name exec (Hash h) =
  [||
  $$fetcher
    $$(liftTyped sys)
    (makeStorePathNameThrow $$(liftTyped name))
    $$(liftTyped exec)
    $$(liftTyped (NH.Hash h))
  ||]

decodeSpec :: Yaml.FromJSON a => FilePath -> Q a
decodeSpec p =
  addDependentFile p
    >> runIO (Yaml.decodeFileThrow p)

mkSupportTools ::
  Name ->
  CodeQ (System -> StorePathName -> Bool -> NH.Hash NH.SHA256 -> d) ->
  System ->
  [SupportTool] ->
  Q [Dec]
mkSupportTools drvType fetcher sys sts = do
  let typ = ConT drvType
  concat
    <$> traverse
      ( \st -> do
          n <-
            let ts = T.split (== '-') (stName st)
             in newName
                  ( T.unpack
                      ( T.concat
                          ( head ts
                              : foldr'
                                ( \t r ->
                                    T.singleton (toUpper (T.head t))
                                      : T.tail t
                                      : r
                                )
                                []
                                (tail ts)
                          )
                      )
                  )
          e <-
            unTypeCode
              ( fetchExp
                  fetcher
                  sys
                  (stName st)
                  (stExecutable st)
                  (stSha256 st)
              )
          pure [SigD n typ, FunD n [Clause [] (NormalB e) []]]
      )
      sts

mkBootDerivations ::
  Name ->
  CodeQ (UnpackDerivArg -> d -> [unpacked] -> unpacked) ->
  System ->
  CodeQ (System -> StorePathName -> Bool -> NH.Hash NH.SHA256 -> d) ->
  [BootDeriv] ->
  DecsQ
mkBootDerivations unpackedT unpackDeriv system fetcher bds =
  do
    let t = ConT unpackedT
    nameMap <-
      HM.fromList
        <$> traverse
          ( \d ->
              (bdOldStorePath d,)
                <$> ( newName
                        . T.unpack
                        . T.map
                          ( \case
                              '-' -> '_'
                              '.' -> '_'
                              c -> c
                          )
                    )
                  (bdName d)
          )
          bds
    concat
      <$> traverse
        ( \bd ->
            let n = fromJust (HM.lookup (bdOldStorePath bd) nameMap)
             in do
                  fmap
                    ( \e ->
                        [ SigD n t,
                          FunD n [Clause [] (NormalB e) []]
                        ]
                    )
                    ( unTypeCode
                        [||
                        $$unpackDeriv
                          $$( liftTyped
                                UnpackDerivArg
                                  { udaName = bdName bd,
                                    udaSystem = system,
                                    udaStoreName = bdStoreName bd,
                                    udaOrigStorePath = bdOldStorePath bd
                                  }
                            )
                          $$( fetchExp
                                fetcher
                                system
                                (daName (bdArchive bd))
                                False
                                (daSha256 (bdArchive bd))
                            )
                          $$( unsafeCodeCoerce
                                ( listE
                                    ( mapMaybe
                                        ( \p ->
                                            if p == bdOldStorePath bd
                                              then Nothing
                                              else
                                                Just
                                                  ( varE
                                                      (fromJust (HM.lookup p nameMap))
                                                  )
                                        )
                                        (bdDependent bd)
                                    )
                                )
                            )
                        ||]
                    )
        )
        bds
