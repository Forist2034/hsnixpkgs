{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.HsBuilder.OutputMod (outputMod) where

import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import HsNix.OutputName
import Language.Haskell.GenPackage
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import System.IO.Unsafe

outputMod ::
  NEL.NonEmpty OutputName ->
  HsModule
    'OtherModule
    (HM.HashMap OutputName (Code HsQ FilePath))
outputMod os =
  otherModule
    "Outputs"
    []
    ( HM.fromList . NEL.toList
        <$> traverse
          ( \s ->
              let us = T.unpack (outputNameText s)
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