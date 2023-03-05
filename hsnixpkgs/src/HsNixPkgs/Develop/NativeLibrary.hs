{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.Develop.NativeLibrary (
  NativeLib (..),
  setupNativeLib,
  nativeLibPaths,
  nativeLib,
) where

import Control.Applicative
import Data.Default
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe
import Data.Singletons
import Data.Text (Text)
import HsNix.Derivation
import HsNix.DrvStr (DrvStr)
import qualified HsNix.DrvStr as DS
import qualified HsNix.DrvStr.Builder as DSB
import HsNixPkgs.Dependent
import HsNixPkgs.ExtendDrv
import HsNixPkgs.SetupHook
import HsNixPkgs.System
import HsNixPkgs.Util

data NativeLib b h t = NativeLib
  { ldFlagBefore :: [DrvStr],
    ldDrvOutput :: DrvOutput h t,
    ldSubDirs :: [Text],
    ldFlagsAfter :: [DrvStr],
    nativeLibDepends :: SimpleDeps [] NativeLib b h t
  }
  deriving (Show)

instance Eq (NativeLib b h t) where
  l == r = ldDrvOutput l == ldDrvOutput r

instance Hashable (NativeLib b h t) where
  hashWithSalt s v = hashWithSalt s (ldDrvOutput v)

instance HasPropagatedDep NativeLib [] where
  propagatedDep = nativeLibDepends

setupNativeLib ::
  forall b h t.
  (SingI b, SingI h, SingI t) =>
  SimpleDeps [] NativeLib b h t ->
  DrvM SetupHook
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
    setupEnv :: [NativeLib b1 h1 t1] -> DrvM ([DrvStr] -> [DrvStr])
    setupEnv ds =
      traverse (\i -> (i,) <$> getStorePathStr (ldDrvOutput i)) ds
        <&> \sps v ->
          foldr'
            ( \(i, sp) s ->
                ldFlagBefore i
                  ++ foldr'
                    (\r l -> "-L" : (sp <> "/" <> DS.fromText r) : l)
                    (ldFlagsAfter i ++ s)
                    (ldSubDirs i)
            )
            v
            sps

    toEnv :: forall (t1 :: System). Sing t1 -> [DrvStr] -> Maybe (Text, DrvStr)
    toEnv _ [] = Nothing
    toEnv s ds =
      Just
        ( "NIX_LDFLAGS_" <> toEnvSuffix (fromSing s),
          DSB.toDrvStr (escapeArgs ds)
        )

nativeLibPaths :: [NativeLib b h t] -> DrvM [DrvStr]
nativeLibPaths =
  fmap concat
    . traverse
      ( \l ->
          getStorePathStr (ldDrvOutput l) <&> \sp ->
            fmap
              (\s -> sp <> "/" <> DS.fromText s)
              (ldSubDirs l)
      )

nativeLib ::
  (SingI b, SingI h, SingI t) =>
  SimpleDeps [] NativeLib b h t ->
  [(SimpleDeps [] NativeLib b h t, DrvM SetupHook)] ->
  DrvM SetupHook
nativeLib d dl =
  let (ds, sh) = collectDepsA d dl
   in liftA2 (<>) (setupNativeLib ds) sh