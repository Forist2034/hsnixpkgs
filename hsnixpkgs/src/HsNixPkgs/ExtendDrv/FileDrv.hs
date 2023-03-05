{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module HsNixPkgs.ExtendDrv.FileDrv where

import Data.Hashable
import Data.Singletons.Base.TH
import Data.Text (Text)
import HsNix.Derivation

singletons
  [d|
    data FileSrcType
      = RegularFSrc
      | DirectoryFSrc
    |]

data FileSource (t :: FileSrcType) = FileSrc
  { fSrcName :: Text,
    fSrcBase :: SrcInput
  }
  deriving (Show, Eq)

instance Hashable (FileSource t) where
  hashWithSalt s v = hashWithSalt s (fSrcBase v)