{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.Boot.Build.Fixup.MultipleOutput
  ( MultiOutput (..),
    multiOutputFixup,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Text as T
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Directory
import System.FilePath
import System.IO.Error

moveToOutput :: FilePath -> FilePath -> FilePath -> IO ()
moveToOutput src dest subDir
  | dest == src = pure ()
  | otherwise =
      let srcPath = src </> subDir
       in doesPathExist srcPath >>= \e ->
            when e $ do
              srcParent <- canonicalizePath (srcPath </> "..")

              let destPath = dest </> subDir
              echo ["Moving ", srcPath, " to ", destPath]
              liftA2 (&&) (doesDirectoryExist srcPath) (doesDirectoryExist destPath)
                >>= \case
                  -- try to merge dir
                  True -> do
                    sl <- listDirectory srcPath
                    unless (null sl) $ do
                      traverse_ (\f -> renamePath (srcPath </> f) (destPath </> f)) sl
                    removeDirectory srcPath
                  False -> do
                    canonicalizePath (takeDirectory destPath)
                      >>= createDirectoryIfMissing True
                    renamePath srcPath destPath

              -- ignore nonexist exceptions
              catch
                ( do
                    np <- null <$> listDirectory srcParent
                    when np $ do
                      echo ["Removing ", srcParent, " and  (possibly) its parents"]
                      removeDirectory srcParent
                      let pp = srcParent </> ".."
                      listDirectory pp >>= \pl ->
                        when (null pl) (removeDirectory pp)
                )
                ( \err ->
                    if isDoesNotExistError err
                      then pure ()
                      else throwIO err
                )

data MultiOutput = MultiOutput
  { outDev :: FilePath,
    outBin :: FilePath,
    outInclude :: FilePath,
    outLib :: FilePath,
    outDoc :: FilePath,
    outDevDoc :: FilePath,
    outMan :: FilePath,
    outDevMan :: FilePath,
    outInfo :: FilePath
  }
  deriving (Show)

-- | Move documentation to the desired outputs.
moveDocs :: MultiOutput -> FilePath -> BIO ()
moveDocs mo o = liftIO $ do
  moveToOutput o (outInfo mo) "share/info"
  moveToOutput o (outDoc mo) "share/doc"
  moveToOutput o (outDevDoc mo) "share/gtk-doc"
  moveToOutput o (outDevDoc mo) "share/devhelp/books"

  moveToOutput o (outDevMan mo) "share/man/man3"
  moveToOutput o (outMan mo) "share/man"

-- | Move development-only stuff to the desired outputs.
moveDev :: MultiOutput -> FilePath -> BIO ()
moveDev mo o | outDev mo == o = pure ()
moveDev MultiOutput {outDev = dev, outInclude = include} o = liftIO $ do
  moveToOutput o include "include"

  moveToOutput o dev "lib/pkgconfig"
  moveToOutput o dev "share/pkgconfig"
  moveToOutput o dev "lib/cmake"
  moveToOutput o dev "share/aclocal"

  listDirectory (dev </> "lib" </> "pkgconfig") >>= patch
  listDirectory (dev </> "share" </> "pkgconfig") >>= patch
  where
    incl = T.pack ('=' : include)
    patch =
      traverse_
        ( \f -> do
            echo ["Patching ", f, " includedir to output ", include]
            modifyTextFile
              ( T.unlines
                  . fmap
                    ( \l ->
                        if "includedir=" `T.isPrefixOf` l
                          then T.replace "=${prefix}" incl l
                          else l
                    )
                  . T.lines
              )
              f
        )
        . filter (".pc" `isExtensionOf`)

multiOutputFixup :: MultiOutput -> FilePath -> BIO ()
multiOutputFixup mo fp = moveDocs mo fp >> moveDev mo fp