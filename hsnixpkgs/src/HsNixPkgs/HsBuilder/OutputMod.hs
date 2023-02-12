{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.HsBuilder.OutputMod (outputMod) where

import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import System.IO.Unsafe

outputMod ::
  IsString s =>
  NEL.NonEmpty Text ->
  HsModule
    s
    'OtherModule
    (HM.HashMap Text (Code HsQ FilePath))
outputMod os =
  declModule
    "Outputs"
    []
    ( HM.fromList . NEL.toList
        <$> traverse
          ( \s ->
              let us = T.unpack s
               in declFun
                    ( newName us >>= \n ->
                        sequence
                          [ sigD n [t|FilePath|],
                            funD
                              n
                              [ clause
                                  []
                                  (normalB [|unsafePerformIO (getEnv $(lift us))|])
                                  []
                              ],
                            pragInlD n NoInline FunLike AllPhases
                          ]
                    )
                    <&> \n -> (s, unsafeCodeCoerce (varE n))
          )
          os
    )