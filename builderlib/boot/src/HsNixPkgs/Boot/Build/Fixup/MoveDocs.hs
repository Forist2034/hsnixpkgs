module HsNixPkgs.Boot.Build.Fixup.MoveDocs (moveToShare) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Directory
import System.FilePath

moveToShare :: [FilePath] -> FilePath -> BIO ()
moveToShare ds p =
  liftIO $
    traverse_
      ( \d ->
          let src = p </> d
              dest = p </> "share" </> d
           in doesPathExist src >>= \se ->
                when se $ do
                  de <- doesPathExist dest
                  if de
                    then echo ["both ", src, " and ", dest, " exist"]
                    else do
                      echo ["moving ", src, " to ", dest]
                      createDirectoryIfMissing True dest
                      renamePath src dest
      )
      ds
