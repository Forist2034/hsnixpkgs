{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.Boot.Build.Fixup.CompressManPages (compressManPages) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as L
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Process

notArchive :: FilePath -> Bool
notArchive f = takeExtension f `notElem` [".bz2", ".gz", ".xz"]

compressManPages :: FilePath -> BIO ()
compressManPages p = liftIO $ do
  e <-
    liftA3
      (\a b c -> a || b || not c)
      (pathIsSymbolicLink (p </> "share"))
      (pathIsSymbolicLink (p </> "share" </> "man"))
      (doesDirectoryExist (p </> "share" </> "man"))
  unless
    e
    ( do
        putStrLn ("gzipping man pages under " ++ manPath)
        listDirRec manPath
          >>= filterM (fmap isRegularFile . getFileStatus)
            . filter notArchive
          >>= traverse_
            ( \f ->
                let dest = f <.> "gz"
                 in withBinaryFile
                      dest
                      WriteMode
                      ( \h ->
                          withCreateProcess
                            ( (proc "gzip" ["-c", "-n", f])
                                { std_out = UseHandle h
                                }
                            )
                            (\_ _ _ ph -> waitForProcess ph)
                            >>= \case
                              ExitSuccess -> removeFile f
                              ExitFailure _ -> removeFile dest
                      )
            )
        listDirRec manPath
          >>= filterM pathIsSymbolicLink
            . filter notArchive
          >>= traverse_
            ( \f -> do
                dest <- canonicalizePath f
                cond <-
                  liftA2
                    (&&)
                    (doesPathExist dest)
                    (isRegularFile <$> getFileStatus dest)
                when cond $
                  mask_
                    ( do
                        createFileLink (dest <.> "gz") (f <.> "gz")
                        removeFile f
                    )
            )
            . L.sort
    )
  where
    manPath = p </> "share" </> "man"