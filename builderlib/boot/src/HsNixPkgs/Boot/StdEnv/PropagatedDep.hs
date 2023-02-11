module HsNixPkgs.Boot.StdEnv.PropagatedDep (Deps (..), writePropagatedDep) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath

data Deps = Deps
  { depsBuildBuild :: [Text],
    -- | same as nixpkgs nativeBuildInputs
    depsBuildHost :: [Text],
    depsBuildTarget :: [Text],
    depsHostHost :: [Text],
    -- | same as nixpkgs buildInputs
    depsHostTarget :: [Text],
    depsTargetTarget :: [Text]
  }

writePropagatedDep :: FilePath -> Deps -> IO ()
writePropagatedDep out d =
  createDirectoryIfMissing True (out </> "nix-support")
    >> withCurrentDirectory
      (out </> "nix-support")
      ( do
          write "propagated-build-build-deps" (depsBuildBuild d)
          write "propagated-native-build-inputs" (depsBuildHost d)
          write "propagated-build-target-deps" (depsBuildTarget d)
          write "propagated-host-host-deps" (depsHostHost d)
          write "propagated-build-inputs" (depsHostTarget d)
          write "propagated-target-target-deps" (depsTargetTarget d)
      )
  where
    write p l = unless (null l) (TIO.writeFile p (T.unlines l))