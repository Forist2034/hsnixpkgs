module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml.Pretty as Yaml
import HsNixPkgs.BootTools.Derivation
import HsNixPkgs.BootTools.ReferenceGraph
import HsNixPkgs.BootTools.SupportTools
import System.Directory
import System.Environment
import System.FilePath

packDeps :: T.Text -> FilePath -> String -> String -> IO ()
packDeps store dest graphEnv exportedEnv = do
  let d = dest </> "derivation"
  createDirectory d
  exported <-
    fmap (\l -> let w = T.words l in (head w, w !! 1)) . T.lines
      <$> (getEnv exportedEnv >>= TIO.readFile)
  getEnv graphEnv
    >>= readRefGraphs exported . L.words
    >>= packDerivations store d . topologicalSort
    >>= BS.writeFile (dest </> "derivation.yaml")
      . Yaml.encodePretty Yaml.defConfig

copyHsTools :: FilePath -> FilePath -> FilePath -> IO ()
copyHsTools dest compileBuilder unpackBootTools = do
  let d = dest </> "hsnixpkgs-support"
  createDirectory d
  cb <- copySupportTool "compile-builder" True (compileBuilder </> "bin") d
  ubt <- copySupportTool "unpack-stdenv-linux" True (unpackBootTools </> "bin") d
  BS.writeFile
    (dest </> "support-tool.yaml")
    (Yaml.encodePretty Yaml.defConfig [cb, ubt])

main :: IO ()
main = do
  [graphEnv, exportedEnv, compileBuildPath, unpackBootToolsPath] <- getArgs
  store <- T.pack <$> getEnv "NIX_STORE"
  dest <- getEnv "out"
  createDirectory dest
  packDeps store dest graphEnv exportedEnv
  copyHsTools dest compileBuildPath unpackBootToolsPath
