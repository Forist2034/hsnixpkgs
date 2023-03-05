module Language.Haskell.GenPackage (
  ModuleType (..),
  HsModule,
  module StrM,
  module SynM,
  HsExecutable (..),
  hsExecutable,
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Language.Haskell.GenPackage.Str
import Language.Haskell.GenPackage.StringMod as StrM
import Language.Haskell.GenPackage.SynMod as SynM
import Language.Haskell.GenPackage.Types

data HsExecutable = HsExecutable
  { execModules :: HM.HashMap String Str,
    execDependencies :: HS.HashSet String
  }

hsExecutable :: HsModule 'MainModule () -> HsExecutable
hsExecutable dm =
  let (om, opd) = evalState (execWriterT (modDepM dm)) HM.empty
   in HsExecutable
        { execModules = om,
          execDependencies = opd
        }