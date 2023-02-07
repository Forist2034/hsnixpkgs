module Main (main) where

import Data.Foldable (traverse_)
import GHC.ResponseFile
import System.Directory
import System.Environment
import System.FilePath
import System.Process

packageConfDir :: FilePath
packageConfDir = "package.conf.d"

sourceDir :: FilePath
sourceDir = "src"

setupPackageConf :: String -> [String] -> IO ()
setupPackageConf ghcPkg ds = do
  createDirectory packageConfDir
  withCurrentDirectory packageConfDir $
    traverse_
      ( \d ->
          listDirectory d
            >>= traverse_ (\c -> copyFile (d </> c) c)
              . filter (".conf" `isExtensionOf`)
      )
      ds
  callProcess ghcPkg ["--package-db=" ++ packageConfDir, "recache"]

writeSources :: [(String, String)] -> IO ()
writeSources srcs =
  createDirectory sourceDir
    >> withCurrentDirectory
      sourceDir
      ( traverse_
          ( \(m, s) -> do
              let dest = fmap (\c -> if c == '.' then '/' else c) m <.> "hs"
              createDirectoryIfMissing True (takeDirectory dest)
              f <- getEnv s
              copyFile f dest
          )
          srcs
      )

main :: IO ()
main = do
  ghc <- getEnv "ghc"
  getEnv "packages"
    >>= setupPackageConf (ghc ++ "-pkg") . words
  getEnv "sources" >>= writeSources . read
  flags <- unescapeArgs <$> getEnv "ghc_flags"
  out <- getEnv "out"
  callProcess
    ghc
    ( flags
        ++ [ "--make",
             "-threaded",
             "-rtsopts",
             "-package-db=" ++ packageConfDir,
             "-i" ++ sourceDir,
             sourceDir </> "Main.hs",
             "-o",
             out
           ]
    )