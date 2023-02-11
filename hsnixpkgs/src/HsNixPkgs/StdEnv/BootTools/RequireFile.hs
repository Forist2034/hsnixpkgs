module HsNixPkgs.StdEnv.BootTools.RequireFile (requireFile) where

import Data.Text (Text)
import HsNix.Derivation
import HsNix.Hash
import HsNix.System

requireFile ::
  ApplicativeDeriv m =>
  System ->
  Text ->
  Bool ->
  Hash SHA256 ->
  Derivation m
requireFile sys n exec h =
  derivation
    ( pure
        ( (defaultDrvArg n "fake-builder" sys)
            { drvType =
                FixedOutput
                  (if exec then HashRecursive else HashFlat)
                  h
            }
        )
    )