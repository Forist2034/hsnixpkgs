{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.Boot.Build.Phase (Phase (..), runPhases) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import GHC.IO.Handle.FD
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import System.Environment
import System.IO

data Phase = Phase
  { phaseName :: Text,
    phaseDesc :: Text,
    phaseFunc :: BIO ()
  }

runPhases :: [Phase] -> BIO ()
runPhases ps = do
  putLog <-
    liftIO
      ( lookupEnv "NIX_LOG_FD" >>= \case
          Just f -> do
            h <- fdToHandle (read f)
            pure
              ( \p ->
                  TIO.hPutStrLn h (mconcat ["@nix { \"action\": \"setPhase\", \"phase\": \"", p, "\""])
                    >> hFlush h
              )
          Nothing -> pure (const (pure ()))
      )
  traverse_
    ( \p -> do
        liftIO $ putLog (phaseName p)
        liftIO $ TIO.putStrLn (phaseDesc p)
        startTime <- liftIO getCurrentTime
        phaseFunc p
        endTime <- liftIO getCurrentTime
        liftIO
          ( TIO.putStr (phaseName p)
              >> echo [" completed in ", show (diffUTCTime endTime startTime)]
          )
    )
    ps