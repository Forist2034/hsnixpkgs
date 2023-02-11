{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.StdEnv.BootTools.TH
  ( decodeSpec,
    mkSupportTools,
    mkBootDerivations,
  )
where

import Data.Char
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import HsNix.Derivation
import qualified HsNix.Hash as NH
import HsNix.System
import HsNixPkgs.BootTools.Derivation
import HsNixPkgs.BootTools.SupportTools
import HsNixPkgs.StdEnv.BootTools.Derivation
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

fetchExp :: ExpQ -> System -> Text -> Bool -> Hash -> ExpQ
fetchExp fetcher sys name exec (Hash h) =
  [|
    $fetcher
      $(lift sys)
      $(lift name)
      $(lift exec)
      $(lift (NH.Hash h))
    |]

decodeSpec :: Yaml.FromJSON a => FilePath -> Q a
decodeSpec p =
  addDependentFile p
    >> runIO (Yaml.decodeFileThrow p)

mkSupportTools :: ExpQ -> System -> [SupportTool] -> DecsQ
mkSupportTools fetcher sys sts = do
  typ <- [t|forall m. ApplicativeDeriv m => Derivation m|]
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
            fetchExp
              fetcher
              sys
              (stName st)
              (stExecutable st)
              (stSha256 st)
          pure [SigD n typ, FunD n [Clause [] (NormalB e) []]]
      )
      sts

mkBootDerivations ::
  ExpQ ->
  System ->
  ExpQ ->
  [BootDeriv] ->
  DecsQ
mkBootDerivations unpack system fetcher bds =
  do
    t <- [t|forall m. ApplicativeDeriv m => UnpackedDeriv m|]
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
                    [|
                      unpackDeriv
                        $unpack
                        $( lift
                             UnpackDerivArg
                               { udaName = bdName bd,
                                 udaSystem = system,
                                 udaStoreName = bdStoreName bd,
                                 udaOrigStorePath = bdOldStorePath bd
                               }
                         )
                        $( fetchExp
                             fetcher
                             system
                             (daName (bdArchive bd))
                             False
                             (daSha256 (bdArchive bd))
                         )
                        $( listE
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
                      |]
        )
        bds
