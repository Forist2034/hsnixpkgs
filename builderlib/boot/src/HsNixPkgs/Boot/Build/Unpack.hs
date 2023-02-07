module HsNixPkgs.Boot.Build.Unpack
  ( unTarXz,
    unTar,
    unpackFunc,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Directory
import System.IO
import System.Process

unTarXz :: FilePath -> IO ()
unTarXz f =
  withBinaryFile
    f
    ReadMode
    ( \hf ->
        pipeCreateProcess
          (UseHandle hf)
          (const (pure ()))
          [ proc "xz" ["-d"],
            proc "tar" ["xf", "-", "--warning=no-timestamp"]
          ]
          Inherit
    )

unTar :: FilePath -> IO ()
unTar f = callProcess "tar" ["xf", f, "--warning=no-timestamp"]

unpackFunc :: [(String, IO ())] -> FilePath -> Bool -> BIO ()
unpackFunc sources sourceRoot makeWritable =
  liftIO
    ( do
        traverse_
          ( \(s, f) ->
              echo ["Unpacking source archive ", s] >> f
          )
          sources
        when makeWritable $
          listDirRec sourceRoot
            >>= traverse_ (changePermissions (setOwnerWritable True))
        changePermissions (setOwnerSearchable True) sourceRoot
        setCurrentDirectory sourceRoot
    )