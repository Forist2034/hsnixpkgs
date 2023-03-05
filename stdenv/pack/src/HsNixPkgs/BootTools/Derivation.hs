{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.BootTools.Derivation (
  Hash (..),
  DerivArchive (..),
  BootDeriv (..),
  packDerivations,
) where

import Codec.Compression.Lzma
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Crypto.Hash
import qualified Data.Aeson as JSON
import Data.Aeson.TH (Options (fieldLabelModifier), deriveJSON)
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HsNixPkgs.BootTools.ReferenceGraph
import HsNixPkgs.BootTools.StorePath
import HsNixPkgs.BootTools.Type
import Nix.Nar
import System.FilePath
import System.OsPath.Posix (encodeUtf)

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

data LogInfo = LogInfo
  { liNameWidth :: Int,
    liNum :: Text
  }

type LogItem = (Text, Int)

writeLog :: MonadIO m => LogInfo -> LogItem -> Text -> m ()
writeLog li (n, i) msg =
  liftIO
    ( TIO.putStrLn
        ( mconcat
            [ "[",
              T.justifyLeft (T.length (liNum li)) ' ' (T.pack (show i)),
              " of ",
              liNum li,
              "] ",
              T.justifyLeft (liNameWidth li) ' ' n,
              "> ",
              msg
            ]
        )
    )

archiveDerivation ::
  LogInfo ->
  LogItem ->
  FilePath ->
  Text ->
  StorePath ->
  PackM (DerivArchive, Text)
archiveDerivation li lt dest p sp = do
  writeLog li lt ("Packing nar of " <> p)
  nar <- encode <$> liftIO (readNar (fromJust (encodeUtf (T.unpack p))))
  writeLog li lt "Compressing nar"
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
      path = dest </> T.unpack filename
  writeLog li lt ("Writing compressed nar to " <> T.pack path)
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
packDerivations store dest gns = do
  TIO.putStrLn "These derivations with be packed:"
  traverse_ (\gn -> TIO.putStrLn ("    " <> storePath gn)) gns
  evalStateT
    ( let withSP =
            zipWith
              ( \idx gn ->
                  let sp = parseStorePath store (storePath gn)
                   in ((storePathName sp, idx), (gn, sp))
              )
              [1, 2 ..]
              gns
          logInfo =
            LogInfo
              { liNameWidth = maximum (fmap (\((n, _), _) -> T.length n) withSP),
                liNum = T.pack (show (length gns))
              }
       in traverse
            ( \(lt, (gn, sp)) ->
                ( \(da, suf) ->
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
                  <$> archiveDerivation
                    logInfo
                    lt
                    dest
                    (storePath gn)
                    sp
            )
            withSP
    )
    HM.empty
