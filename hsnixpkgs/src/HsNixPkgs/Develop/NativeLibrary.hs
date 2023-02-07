{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.Develop.NativeLibrary
  ( NativeLib (..),
    setupNativeLib,
    nativeLibPaths,
    nativeLib,
  )
where

import Control.Applicative
import Data.Default
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe
import Data.Singletons
import Data.Text (Text)
import HsNixPkgs.Dependent
import HsNixPkgs.ExtendDrv
import HsNixPkgs.SetupHook
import HsNixPkgs.System
import HsNixPkgs.Util

data NativeLib b h t m = NativeLib
  { ldFlagBefore :: [DrvStr m],
    ldDrvOutput :: DrvOutput h t m,
    ldSubDirs :: [Text],
    ldFlagsAfter :: [DrvStr m],
    nativeLibDepends :: SimpleDeps [] NativeLib b h t m
  }

deriving instance (ApplicativeDeriv m) => Show (NativeLib b h t m)

deriving instance (ApplicativeDeriv m) => Eq (NativeLib b h t m)

instance (ApplicativeDeriv m) => Hashable (NativeLib b h t m) where
  hashWithSalt s v = hashWithSalt s (ldDrvOutput v)

instance HasPropagatedDep NativeLib [] where
  propagatedDep = nativeLibDepends

setupNativeLib ::
  forall m b h t.
  (SingI b, SingI h, SingI t, ApplicativeDeriv m) =>
  SimpleDeps [] NativeLib b h t m ->
  m (SetupHook m)
setupNativeLib sd = do
  eb <-
    toEnv (sing @b)
      <$> ( pure []
              <**> setupEnv (depsBuildBuild sd)
              <**> setupEnv (depsBuildHost sd)
              <**> setupEnv (depsBuildTarget sd)
          )
  eh <-
    toEnv (sing @h)
      <$> ( pure []
              <**> setupEnv (depsHostHost sd)
              <**> setupEnv (depsHostTarget sd)
          )
  et <- toEnv (sing @t) <$> (setupEnv (depsTargetTarget sd) <*> pure [])
  pure
    ( def
        { newEnv =
            HM.fromListWith
              (\l r -> l <> " " <> r)
              (catMaybes [eb, eh, et])
        }
    )
  where
    setupEnv :: [NativeLib b1 h1 t1 m] -> m ([DrvStr m] -> [DrvStr m])
    setupEnv ds =
      traverse (\i -> (i,) <$> getStorePathStr (ldDrvOutput i)) ds
        <&> \sps v ->
          foldr'
            ( \(i, sp) s ->
                ldFlagBefore i
                  ++ foldr'
                    (\r l -> "-L" : (sp <> "/" <> toDrvStr r) : l)
                    (ldFlagsAfter i ++ s)
                    (ldSubDirs i)
            )
            v
            sps

    toEnv :: forall (t1 :: System). Sing t1 -> [DrvStr m] -> Maybe (Text, DrvStr m)
    toEnv _ [] = Nothing
    toEnv s ds =
      Just
        ( "NIX_LDFLAGS_" <> toEnvSuffix (fromSing s),
          toDrvStr (escapeArgs ds)
        )

nativeLibPaths :: ApplicativeDeriv m => [NativeLib b h t m] -> m [DrvStr m]
nativeLibPaths =
  fmap concat
    . traverse
      ( \l ->
          getStorePathStr (ldDrvOutput l) <&> \sp ->
            fmap
              (\s -> sp <> "/" <> toDrvStr s)
              (ldSubDirs l)
      )

nativeLib ::
  (ApplicativeDeriv m, SingI b, SingI h, SingI t) =>
  SimpleDeps [] NativeLib b h t m ->
  [(SimpleDeps [] NativeLib b h t m, m (SetupHook m))] ->
  m (SetupHook m)
nativeLib d dl =
  let (ds, sh) = collectDepsA d dl
   in liftA2 (<>) (setupNativeLib ds) sh