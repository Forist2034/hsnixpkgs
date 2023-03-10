module Main (main) where

import Codec.Compression.Lzma
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import qualified Data.Text.IO as TIO
import HsNixPkgs.BootTools.HashRewrite
import Nix.Nar
import System.Environment
import System.OsPath.Posix

withLog :: String -> a -> IO a
withLog s v = putStrLn s $> v

main :: IO ()
main = do
  [narXzFile, rewritesFEnv, destEnv] <- getArgs
  rewrites <-
    getEnv rewritesFEnv
      >>= TIO.readFile
      >>= decodeRewriteSpec
  dest <- getEnv destEnv >>= encodeUtf
  LBS.readFile narXzFile
    >>= withLog ("Decompressing nar file " ++ narXzFile)
    >>= withLog "Rewriting hash" . decompress
    >>= rewriteLBSVerbose rewrites
    >>= withLog "Unpacking nar file"
    >>= writeNar dest
      . decode
      . LBS.fromStrict