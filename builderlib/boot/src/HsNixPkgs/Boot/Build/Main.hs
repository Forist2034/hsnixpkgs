module HsNixPkgs.Boot.Build.Main (BuildEnv (..), BIO, runBuildMain) where

import Control.Monad.Reader
import GHC.Conc (getNumProcessors)
import System.Environment

newtype BuildEnv = BuildEnv
  { beBuildCores :: Int
  }
  deriving (Show)

type BIO = ReaderT BuildEnv IO

runBuildMain :: BIO () -> IO ()
runBuildMain mb = do
  bc <-
    lookupEnv "NIX_BUILD_CORES" >>= \e ->
      case maybe 0 read e of
        0 -> getNumProcessors
        n -> pure n
  runReaderT mb (BuildEnv bc)