module HsNixPkgs.Boot.Build.Fixup.Move
  ( moveLib64,
    moveSbin,
    moveSystemdUserUnits,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Directory
import System.FilePath

move :: FilePath -> FilePath -> FilePath -> BIO ()
move from to p =
  let src = p </> from
      dest = p </> to
   in liftIO $
        liftA2 (||) (not <$> doesPathExist src) (pathIsSymbolicLink dest) >>= \cond ->
          unless cond $ do
            echo ["moving ", src, " to ", dest]
            createDirectoryIfMissing True dest
            listDirectory src
              >>= traverse_ (\f -> renamePath (src </> f) (dest </> f))
            removeDirectory src
            createDirectoryLink dest src

moveLib64 :: FilePath -> BIO ()
moveLib64 = move "lib64" "lib"

moveSbin :: FilePath -> BIO ()
moveSbin = move "sbin" "bin"

moveSystemdUserUnits :: FilePath -> BIO ()
moveSystemdUserUnits = move "lib/systemd/user" "share/systemd/user"