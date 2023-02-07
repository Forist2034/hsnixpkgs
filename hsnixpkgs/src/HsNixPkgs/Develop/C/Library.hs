{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.Develop.C.Library
  ( CLibrary (..),
    setupCLibrary,
    cLibrary,
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
import HsNixPkgs.Develop.NativeLibrary
import HsNixPkgs.ExtendDrv
import HsNixPkgs.SetupHook
import HsNixPkgs.System
import HsNixPkgs.Util

data CLibrary b h t m = CLibrary
  { cLibDrv :: DrvOutput h t m,
    cLibIncludeDir :: [Text],
    cCompFlagsBefore :: [DrvStr m],
    cCompFlagsAfter :: [DrvStr m],
    cLibDepends :: SimpleDeps [] CLibrary b h t m,
    cLibNativeLib :: NativeLib b h t m
  }

deriving instance (ApplicativeDeriv m) => Show (CLibrary b h t m)

deriving instance (ApplicativeDeriv m) => Eq (CLibrary b h t m)

instance (ApplicativeDeriv m) => Hashable (CLibrary b h t m) where
  hashWithSalt s a = hashWithSalt s (cLibDrv a)

instance HasPropagatedDep CLibrary [] where
  propagatedDep = cLibDepends

setupCLibrary ::
  forall b h t m.
  (SingI b, SingI h, SingI t, ApplicativeDeriv m) =>
  SimpleDeps [] CLibrary b h t m ->
  (SimpleDeps [] NativeLib b h t m, m (SetupHook m))
setupCLibrary ds =
  ( SimpleDeps
      { depsBuildBuild = fmap cLibNativeLib (depsBuildBuild ds),
        depsBuildHost = fmap cLibNativeLib (depsBuildHost ds),
        depsBuildTarget = fmap cLibNativeLib (depsBuildTarget ds),
        depsHostHost = fmap cLibNativeLib (depsHostHost ds),
        depsHostTarget = fmap cLibNativeLib (depsHostTarget ds),
        depsTargetTarget = fmap cLibNativeLib (depsTargetTarget ds)
      },
    do
      eb <-
        toEnv (sing @b)
          <$> ( pure []
                  <**> setupEnv (depsBuildBuild ds)
                  <**> setupEnv (depsBuildHost ds)
                  <**> setupEnv (depsBuildTarget ds)
              )
      eh <-
        toEnv (sing @h)
          <$> ( pure []
                  <**> setupEnv (depsHostHost ds)
                  <**> setupEnv (depsHostTarget ds)
              )
      et <- toEnv (sing @t) <$> (setupEnv (depsTargetTarget ds) <*> pure [])
      pure
        ( def
            { newEnv = HM.fromListWith (\l r -> l <> " " <> r) (catMaybes [eb, eh, et])
            }
        )
  )
  where
    setupEnv :: [CLibrary b1 h1 t1 m] -> m ([DrvStr m] -> [DrvStr m])
    setupEnv ls =
      traverse (\l -> (l,) <$> getStorePathStr (cLibDrv l)) ls <&> \lds v ->
        foldr'
          ( \(l, d) e ->
              cCompFlagsBefore l
                ++ foldr'
                  (\p el -> "-isystem" : (d <> "/" <> toDrvStr p) : el)
                  (cCompFlagsAfter l ++ e)
                  (cLibIncludeDir l)
          )
          v
          lds
    toEnv :: forall (t1 :: System). Sing t1 -> [DrvStr m] -> Maybe (Text, DrvStr m)
    toEnv _ [] = Nothing
    toEnv s es =
      Just
        ( "NIX_CFLAGS_COMPILE_" <> toEnvSuffix (fromSing s),
          toDrvStr (escapeArgs es)
        )

cLibrary ::
  (SingI b, SingI h, SingI t, ApplicativeDeriv m) =>
  SimpleDeps [] CLibrary b h t m ->
  [(SimpleDeps [] CLibrary b h t m, m (SetupHook m))] ->
  (SimpleDeps [] NativeLib b h t m, m (SetupHook m))
cLibrary d ds =
  let (cd, sh) = collectDepsA d ds
      (nd, shc) = setupCLibrary cd
   in (nd, liftA2 (<>) shc sh)