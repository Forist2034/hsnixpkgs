{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.BootTools.Derivation
  ( Hash (..),
    DerivArchive (..),
    BootDeriv (..),
    packDerivations,
  )
where

import Codec.Compression.Lzma
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Crypto.Hash
import qualified Data.Aeson as JSON
import Data.Aeson.TH (Options (fieldLabelModifier), deriveJSON)
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import HsNixPkgs.BootTools.ReferenceGraph
import HsNixPkgs.BootTools.StorePath
import HsNixPkgs.BootTools.Type
import Nix.Nar
import System.FilePath

data DerivArchive = DerivArchive
  { daName :: Text,
    daSha256 :: Hash,
    daUncompressedSize :: Word64
  }

deriveJSON
  JSON.defaultOptions {fieldLabelModifier = JSON.camelTo2 '-' . drop 2}
  ''DerivArchive

data BootDeriv = BootDeriv
  { bdName :: Text,
    bdStoreName :: Text,
    bdExported :: Bool,
    bdArchive :: DerivArchive,
    bdOldStorePath :: Text,
    bdDependent :: [Text]
  }

deriveJSON
  JSON.defaultOptions {fieldLabelModifier = JSON.camelTo2 '-' . drop 2}
  ''BootDeriv

type PackM = StateT (HM.HashMap Text (Digest SHA256)) IO

archiveDerivation :: FilePath -> Text -> StorePath -> PackM (DerivArchive, Text)
archiveDerivation dest p sp = do
  liftIO (TIO.putStrLn ("Packing nar of " <> p))
  nar <- encode <$> liftIO (readNar (TE.encodeUtf8 p))
  liftIO (TIO.putStrLn "Compressing nar")
  let compressed =
        compressWith
          ( defaultCompressParams
              { compressLevel = CompressionLevel9,
                compressLevelExtreme = True
              }
          )
          nar
      hsh = hashlazy compressed
  hash_suf <- getName 5 (T.pack (show hsh)) hsh
  let filename = storePathName sp <> "-" <> hash_suf <> ".nar.xz"
  liftIO (TIO.putStrLn "Writing compressed nar to output")
  liftIO (LBS.writeFile (dest </> T.unpack filename) compressed)
  pure
    ( DerivArchive
        { daName = filename,
          daSha256 = Hash hsh,
          daUncompressedSize = fromIntegral (LBS.length nar)
        },
      hash_suf
    )
  where
    getName :: Int -> Text -> Digest SHA256 -> PackM Text
    getName n t d =
      let short = T.take n t
       in gets (HM.member short) >>= \case
            True -> getName (n + 5) t d
            False ->
              modify (HM.insert short d)
                $> short

packDerivations :: Text -> FilePath -> [GraphNode] -> IO [BootDeriv]
packDerivations store dest gns =
  evalStateT
    ( traverse
        ( \gn ->
            let sp = parseStorePath store (storePath gn)
             in ( \(da, suf) ->
                    BootDeriv
                      { bdName =
                          fromMaybe
                            (storePathName sp <> "-" <> suf)
                            (exportedName gn),
                        bdStoreName = storePathName sp,
                        bdExported = isJust (exportedName gn),
                        bdArchive = da,
                        bdOldStorePath = storePath gn,
                        bdDependent = dependencies gn
                      }
                )
                  <$> archiveDerivation dest (storePath gn) sp
        )
        gns
    )
    HM.empty
