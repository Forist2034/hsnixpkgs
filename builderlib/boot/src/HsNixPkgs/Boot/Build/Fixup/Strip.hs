{-# LANGUAGE DeriveLift #-}

module HsNixPkgs.Boot.Build.Fixup.Strip
  ( StripCfg (..),
    stripDirs,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as L
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import Language.Haskell.TH.Syntax (Lift)
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Process

data StripCfg = StripCfg
  { stripCmd :: FilePath,
    ranlibCmd :: FilePath
  }
  deriving (Show, Lift)

stripDirs :: StripCfg -> [String] -> [FilePath] -> FilePath -> BIO ()
stripDirs StripCfg {stripCmd = strip, ranlibCmd = ranlib} sa d p =
  liftIO
    ( filterM doesPathExist (fmap (p </>) d)
        >>= traverse_
          ( \fp -> do
              echo ["stripping (with command ", strip, " and flags ", show sa, ") in ", fp]
              dir <-
                listDirRec fp
                  >>= filterM (fmap isRegularFile . getFileStatus)
              traverse_
                (\f -> callProcess strip (f : sa))
                ( let libDebug = p </> "lib" </> "debug"
                   in filter (not . (libDebug `L.isPrefixOf`)) dir
                )
              {-'strip' does not normally preserve archive index in .a files.
              This usually causes linking failures against static libs like:
                ld: ...-i686-w64-mingw32-stage-final-gcc-13.0.0-lib/i686-w64-mingw32/lib/libstdc++.dll.a:
                  error adding symbols: archive has no index; run ranlib to add one
              Restore the index by running 'ranlib'-}
              traverse_
                (\f -> callProcess ranlib [f])
                (filter (".a" `isExtensionOf`) dir)
          )
    )