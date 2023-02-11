module HsNixPkgs.BootTools.Type (Hash (..)) where

import Crypto.Hash
import Data.Aeson
import qualified Data.Text as T

newtype Hash = Hash (Digest SHA256)

instance ToJSON Hash where
  toJSON (Hash h) = toJSON (show h)

instance FromJSON Hash where
  parseJSON = withText "hash" (pure . Hash . read . T.unpack)
