{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Main (
  B.BIO,
  B.BuildEnv (..),
  runBuildMain,
) where

import qualified HsNixPkgs.Boot.Build.Main as B
import Language.Haskell.GenPackage
import Language.Haskell.TH

runBuildMain :: Code HsQ (B.BIO ()) -> Code HsQ (IO ())
runBuildMain e = [||B.runBuildMain $$e||]