{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.BootTools.Types (
  Hash (..),
  SupportTool (..),
  DerivArchive (..),
  BootDeriv (..),
) where

import Crypto.Hash
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

newtype Hash = Hash (Digest SHA256)
  deriving (Show, Eq)

instance ToJSON Hash where
  toJSON (Hash h) = toJSON (show h)

instance FromJSON Hash where
  parseJSON = withText "hash" (pure . Hash . read . T.unpack)

data SupportTool = SupportTool
  { stName :: Text,
    stExecutable :: Bool,
    stSha256 :: Hash
  }
  deriving (Show)

deriveJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''SupportTool

data DerivArchive = DerivArchive
  { daName :: Text,
    daSha256 :: Hash,
    daUncompressedSize :: Word64
  }
  deriving (Show)

deriveJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''DerivArchive

data BootDeriv = BootDeriv
  { bdName :: Text,
    bdStoreName :: Text,
    bdExported :: Bool,
    bdArchive :: DerivArchive,
    bdOldStorePath :: Text,
    bdDependent :: [Text]
  }
  deriving (Show)

deriveJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''BootDeriv