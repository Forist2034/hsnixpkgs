{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.SetupHook.Executable (
  Executable (..),
  executableDirs,
  executableEnv,
  setupExecutable,
  executable,
) where

import Control.Applicative
import Data.Default
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Singletons
import Data.Text (Text)
import HsNix.Derivation
import qualified HsNix.DrvStr.Builder as DSB
import HsNixPkgs.Dependent
import HsNixPkgs.ExtendDrv
import HsNixPkgs.SetupHook

data Executable b h t = Executable
  { execDrvOutput :: DrvOutput h t,
    execSubDir :: [Text],
    execDep :: SimpleDeps [] Executable b h t
  }
  deriving (Show)

instance Eq (Executable b h t) where
  l == r = execDrvOutput l == execDrvOutput r

instance Hashable (Executable b h t) where
  hashWithSalt s e = hashWithSalt s (execDrvOutput e)

instance HasPropagatedDep Executable [] where
  propagatedDep = execDep

executableDirs ::
  forall b h t.
  SimpleDeps [] Executable b h t ->
  DrvM ([DSB.Builder], [DSB.Builder], [DSB.Builder])
executableDirs d = do
  eb <-
    pure []
      <**> setupExec (depsBuildBuild d)
      <**> setupExec (depsBuildHost d)
      <**> setupExec (depsBuildTarget d)

  eh <-
    pure []
      <**> setupExec (depsHostHost d)
      <**> setupExec (depsHostTarget d)

  et <- setupExec (depsTargetTarget d) <*> pure []
  pure (eb, eh, et)
  where
    setupExec ::
      [Executable b1 h1 t1] ->
      DrvM ([DSB.Builder] -> [DSB.Builder])
    setupExec es =
      traverse
        ( \ex ->
            (,execSubDir ex)
              <$> getStorePathStr (execDrvOutput ex)
        )
        es
        <&> \exs v ->
          foldr'
            ( \(p, sds) i ->
                foldr'
                  ( \sd ->
                      ( ( DSB.fromDrvStr p
                            <> "/"
                            <> DSB.fromText sd
                        )
                          :
                      )
                  )
                  i
                  sds
            )
            v
            exs

executableEnv ::
  [DSB.Builder] ->
  [DSB.Builder] ->
  [DSB.Builder] ->
  SetupHook
executableEnv eb eh et =
  def
    { newEnv =
        HM.fromList
          [ ("PATH", DSB.toDrvStr (toEnv eb)),
            ("HOST_PATH", DSB.toDrvStr (toEnv eh)),
            ("TARGET_PATH", DSB.toDrvStr (toEnv et))
          ]
    }
  where
    toEnv :: [DSB.Builder] -> DSB.Builder
    toEnv [] = mempty
    toEnv (x : xs) = x <> ":" <> toEnv xs

setupExecutable :: SimpleDeps [] Executable b h t -> DrvM SetupHook
setupExecutable =
  fmap (\(eb, eh, et) -> executableEnv eb eh et)
    . executableDirs

executable ::
  forall b h t.
  (SingI b, SingI h) =>
  SimpleDeps [] Executable b h t ->
  DrvM SetupHook
executable es = setupExecutable (propagateDependencies [es])