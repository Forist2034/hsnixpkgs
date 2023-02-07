module HsNixPkgs.Boot.Build.Fixup
  ( makeWritable,
    runFixup,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Directory

makeWritable :: [FilePath] -> BIO ()
makeWritable =
  liftIO
    . traverse_
      ( listDirRec
          >=> traverse_ (changePermissions (setOwnerWritable True))
      )

runFixup :: FilePath -> [FilePath -> BIO ()] -> BIO ()
runFixup p = traverse_ (\f -> f p)