module HsNixPkgs.Boot.Build.Fixup.MakeSymlinkRelative (makeSymlinkRelative) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as L
import HsNixPkgs.Boot.Build.Fixup.MakeSymlinkRelative.FilePath
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Directory
import System.FilePath

makeSymlinkRelative :: FilePath -> BIO ()
makeSymlinkRelative f =
  liftIO
    ( listDirRec f
        >>= filterM pathIsSymbolicLink
        >>= traverse_
          ( \p ->
              getSymbolicLinkTarget p >>= \dest ->
                when (f `L.isPrefixOf` dest) $ do
                  exist <- doesPathExist dest
                  if exist
                    then
                      makeRelativeEx (takeDirectory p) dest
                        >>= maybe
                          (pure ())
                          ( \rd -> do
                              echo ["rewriting symlink ", p, " to be relative to ", f]
                              removeFile p
                              createFileLink rd p
                          )
                    else echo ["the symlink ", p, "is broken, it points to ", dest, " (which is missing)"]
          )
    )