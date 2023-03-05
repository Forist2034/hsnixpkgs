{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module HsNixPkgs.Dependent.Types (
  CA,
  SimpleDeps (..),
  HasPropagatedDep (..),
) where

import Data.Kind
import HsNixPkgs.System

type CA ::
  (Type -> Constraint) ->
  (System -> System -> System -> Type) ->
  Constraint
type CA c a = forall b h t. c (a b h t)

data SimpleDeps c a (b :: System) (h :: System) (t :: System) = SimpleDeps
  { depsBuildBuild :: c (a b b b),
    -- | same as nixpkgs nativeBuildInputs
    depsBuildHost :: c (a b b h),
    depsBuildTarget :: c (a b b t),
    depsHostHost :: c (a b h h),
    -- | same as nixpkgs buildInputs
    depsHostTarget :: c (a b h t),
    depsTargetTarget :: c (a b t t)
  }

deriving instance
  (CA Eq a, (forall e. (Eq e) => Eq (c e))) =>
  Eq (SimpleDeps c a b h t)

deriving instance
  (CA Show a, (forall e. (Show e) => Show (c e))) =>
  Show (SimpleDeps c a b h t)

instance
  (forall b1 h1 t1. Semigroup (c (a b1 h1 t1))) =>
  Semigroup (SimpleDeps c a b h t)
  where
  l <> r =
    SimpleDeps
      { depsBuildBuild = depsBuildBuild l <> depsBuildBuild r,
        depsBuildHost = depsBuildHost l <> depsBuildHost r,
        depsBuildTarget = depsBuildTarget l <> depsBuildTarget r,
        depsHostHost = depsHostHost l <> depsHostHost r,
        depsHostTarget = depsHostTarget l <> depsHostTarget r,
        depsTargetTarget = depsTargetTarget l <> depsTargetTarget r
      }

instance
  (forall b1 h1 t1. Monoid (c (a b1 h1 t1))) =>
  Monoid (SimpleDeps c a b h t)
  where
  mempty =
    SimpleDeps
      { depsBuildBuild = mempty,
        depsBuildHost = mempty,
        depsBuildTarget = mempty,
        depsHostHost = mempty,
        depsHostTarget = mempty,
        depsTargetTarget = mempty
      }

class (Foldable c) => HasPropagatedDep a c | a -> c where
  propagatedDep :: a b h t -> SimpleDeps c a b h t
