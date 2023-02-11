{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.BootTools.SupportTools
  ( SupportTool (..),
    copySupportTool,
  )
where

import Crypto.Hash
import qualified Data.Aeson as JSON
import Data.Aeson.TH (Options (fieldLabelModifier), deriveJSON)
import Data.Binary
import qualified Data.ByteString as BS
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import HsNixPkgs.BootTools.Type
import Nix.Nar
import System.Directory
import System.FilePath

data SupportTool = SupportTool
  { stName :: Text,
    stExecutable :: Bool,
    stSha256 :: Hash
  }

deriveJSON
  JSON.defaultOptions {fieldLabelModifier = JSON.camelTo2 '-' . drop 2}
  ''SupportTool

copySupportTool :: Text -> Bool -> FilePath -> FilePath -> IO SupportTool
copySupportTool name exec src dest =
  let n = T.unpack name
   in do
        copyFile (src </> n) (dest </> n)
        hsh <-
          BS.readFile (src </> n) <&> \c ->
            if exec
              then hashlazy (encode (Nar (Regular Executable c)))
              else hash c
        pure
          ( SupportTool
              { stName = name,
                stExecutable = exec,
                stSha256 = Hash hsh
              }
          )