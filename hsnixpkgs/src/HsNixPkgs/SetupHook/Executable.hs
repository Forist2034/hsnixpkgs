{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.SetupHook.Executable
  ( Executable (..),
    executableDirs,
    executableEnv,
    setupExecutable,
    executable,
  )
where

import Control.Applicative
import Data.Default
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Singletons
import Data.Text (Text)
import HsNixPkgs.Dependent
import HsNixPkgs.ExtendDrv
import HsNixPkgs.SetupHook

data Executable b h t m = Executable
  { execDrvOutput :: DrvOutput h t m,
    execSubDir :: [Text],
    execDep :: SimpleDeps [] Executable b h t m
  }

deriving instance (ApplicativeDeriv m) => Show (Executable b h t m)

deriving instance (ApplicativeDeriv m) => Eq (Executable b h t m)

instance (ApplicativeDeriv m) => Hashable (Executable b h t m) where
  hashWithSalt s e = hashWithSalt s (execDrvOutput e)

instance HasPropagatedDep Executable [] where
  propagatedDep = execDep

executableDirs ::
  forall b h t m.
  ApplicativeDeriv m =>
  SimpleDeps [] Executable b h t m ->
  m ([DrvStrBuilder m], [DrvStrBuilder m], [DrvStrBuilder m])
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
      [Executable b1 h1 t1 m] ->
      m ([DrvStrBuilder m] -> [DrvStrBuilder m])
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
                      ( ( fromDrvStr p
                            <> "/"
                            <> fromDrvStr (toDrvStr sd)
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
  forall m.
  ApplicativeDeriv m =>
  [DrvStrBuilder m] ->
  [DrvStrBuilder m] ->
  [DrvStrBuilder m] ->
  SetupHook m
executableEnv eb eh et =
  def
    { newEnv =
        HM.fromList
          [ ("PATH", toDrvStr (toEnv eb)),
            ("HOST_PATH", toDrvStr (toEnv eh)),
            ("TARGET_PATH", toDrvStr (toEnv et))
          ]
    }
  where
    toEnv :: [DrvStrBuilder m] -> DrvStrBuilder m
    toEnv [] = mempty
    toEnv (x : xs) = x <> ":" <> toEnv xs

setupExecutable ::
  ApplicativeDeriv m =>
  SimpleDeps [] Executable b h t m ->
  m (SetupHook m)
setupExecutable =
  fmap (\(eb, eh, et) -> executableEnv eb eh et)
    . executableDirs

executable ::
  forall b h t m.
  (SingI b, SingI h, ApplicativeDeriv m) =>
  SimpleDeps [] Executable b h t m ->
  m (SetupHook m)
executable es = setupExecutable (propagateDependencies [es])