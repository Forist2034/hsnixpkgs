{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.BootTools.Derivation (packDerivations) where

import Codec.Compression.Lzma
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Crypto.Hash
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Functor
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HsNixPkgs.BootTools.ReferenceGraph
import HsNixPkgs.BootTools.StorePath
import HsNixPkgs.BootTools.Types
import Nix.Nar
import System.FilePath
import System.OsPath.Posix (encodeUtf)

newtype LogInfo = LogInfo
  { liNum :: Text
  }

data LogMessage = LogMessage
  { lmId :: Int,
    lmName :: Text,
    lmMsg :: Text
  }

logger :: LogInfo -> Chan (Maybe LogMessage) -> IO ()
logger li c =
  readChan c >>= \case
    Just msg ->
      TIO.putStrLn
        ( mconcat
            [ "[",
              T.justifyRight (T.length (liNum li)) ' ' (T.pack (show (lmId msg))),
              " of ",
              liNum li,
              "] ",
              lmName msg,
              "> ",
              lmMsg msg
            ]
        )
        >> logger li c
    Nothing -> pure ()

data ArchiveArg = ArchiveArg
  { aaId :: Int,
    aaNode :: GraphNode,
    aaSP :: StorePath,
    aaHashSuf :: Text
  }

archiveDerivation ::
  Chan (Maybe LogMessage) ->
  FilePath ->
  ArchiveArg ->
  IO DerivArchive
archiveDerivation lg dest aa = do
  let p = storePath (aaNode aa)
  writeLog ("Packing nar of " <> p)
  nar <- encode <$> liftIO (readNar (fromJust (encodeUtf (T.unpack p))))
  writeLog "Compressing nar"
  let compressed =
        compressWith
          ( defaultCompressParams
              { compressLevel = CompressionLevel9,
                compressLevelExtreme = True
              }
          )
          nar
      hsh = hashlazy compressed
  let filename = storePathName (aaSP aa) <> "-" <> aaHashSuf aa <> ".nar.xz"
      path = dest </> T.unpack filename
  writeLog ("Writing compressed nar to " <> T.pack path)
  liftIO (LBS.writeFile (dest </> T.unpack filename) compressed)
  pure
    DerivArchive
      { daName = filename,
        daSha256 = Hash hsh,
        daUncompressedSize = fromIntegral (LBS.length nar)
      }
  where
    writeLog m =
      writeChan
        lg
        ( Just
            LogMessage
              { lmId = aaId aa,
                lmName = storePathName (aaSP aa),
                lmMsg = m
              }
        )

toArchiveArg :: Text -> [GraphNode] -> [ArchiveArg]
toArchiveArg store gns =
  evalState
    ( zipWithM
        ( \idx gn ->
            let sp = parseStorePath store (storePath gn)
             in getName 5 (storePathHash sp) <&> \suf ->
                  ArchiveArg
                    { aaId = idx,
                      aaNode = gn,
                      aaSP = sp,
                      aaHashSuf = suf
                    }
        )
        [1, 2 ..]
        gns
    )
    HS.empty
  where
    getName :: Int -> Text -> State (HS.HashSet Text) Text
    getName n t =
      let short = T.take n t
       in gets (HS.member short) >>= \case
            True -> getName (n + 5) t
            False ->
              modify (HS.insert short)
                $> short

packDerivations :: Text -> FilePath -> [GraphNode] -> IO [BootDeriv]
packDerivations store dest gns = do
  TIO.putStrLn "These derivations with be packed:"
  traverse_ (\gn -> TIO.putStrLn ("    " <> storePath gn)) gns

  lc <- newChan
  void
    ( forkIO
        ( logger LogInfo {liNum = T.pack (show (length gns))} lc
        )
    )

  mapConcurrently
    ( \aa@ArchiveArg {aaSP = sp, aaNode = gn} ->
        ( \da ->
            BootDeriv
              { bdName =
                  fromMaybe
                    (storePathName sp <> "-" <> aaHashSuf aa)
                    (exportedName gn),
                bdStoreName = storePathName sp,
                bdExported = isJust (exportedName gn),
                bdArchive = da,
                bdOldStorePath = storePath gn,
                bdDependent = dependencies gn
              }
        )
          <$> archiveDerivation
            lc
            dest
            aa
    )
    (toArchiveArg store gns)
    <* writeChan lc Nothing
