{-# LANGUAGE LambdaCase #-}

module Language.Haskell.GenPackage.Types (
  ModuleType (..),
  PkgDep,
  DepM,
  addDep,
  HsModule (..),
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Language.Haskell.GenPackage.Str

data ModuleType
  = MainModule
  | OtherModule

type PkgDep = (HM.HashMap String Str, HS.HashSet String)

type DepM = WriterT PkgDep (State (HM.HashMap String Bool))

addDep :: String -> PkgDep -> DepM () -> DepM ()
addDep name val dm =
  gets (HM.lookup name) >>= \case
    Just True -> pure ()
    Just False -> error "cycle detected in module dependency graph"
    Nothing -> dm >> tell val >> modify (HM.insert name True)

data HsModule (t :: ModuleType) a = HSModule
  { modDepM :: DepM (),
    modVal :: a
  }