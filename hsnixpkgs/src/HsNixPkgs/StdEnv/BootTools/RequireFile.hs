module HsNixPkgs.StdEnv.BootTools.RequireFile (requireFile) where

import HsNix.Derivation
import HsNix.Hash
import HsNix.StorePathName
import HsNix.System

requireFile ::
  System ->
  StorePathName ->
  Bool ->
  Hash SHA256 ->
  Derivation
requireFile sys n exec h =
  derivation
    ( pure
        ( (defaultDrvArg n "fake-builder" sys)
            { drvType =
                FixedOutput
                  { drvImpureEnvs = [],
                    drvHashMode = if exec then HashRecursive else HashFlat,
                    drvHash = h
                  }
            }
        )
    )