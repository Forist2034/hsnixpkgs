{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.MoveDocs (ForceShare (..), moveToShare) where

import Data.Default
import qualified HsNixPkgs.Boot.Build.Fixup.MoveDocs as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

newtype ForceShare = ForceShare [FilePath]
  deriving (Show)

instance Default ForceShare where
  def = ForceShare ["man", "doc", "info"]

moveToShare :: ForceShare -> Code HsQ (FilePath -> BIO ())
moveToShare (ForceShare fs) = unsafeCodeCoerce [|B.moveToShare $(lift fs)|]