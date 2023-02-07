{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module HsNixPkgs.Dependent.Types
  ( CA,
    SimpleDeps (..),
    HasPropagatedDep (..),
  )
where

import Data.Kind
import HsNixPkgs.System

type CA ::
  (Type -> Constraint) ->
  (System -> System -> System -> k -> Type) ->
  k ->
  Constraint
type CA c a m = forall b h t. c (a b h t m)

data SimpleDeps c a (b :: System) (h :: System) (t :: System) (m :: k) = SimpleDeps
  { depsBuildBuild :: c (a b b b m),
    -- | same as nixpkgs nativeBuildInputs
    depsBuildHost :: c (a b b h m),
    depsBuildTarget :: c (a b b t m),
    depsHostHost :: c (a b h h m),
    -- | same as nixpkgs buildInputs
    depsHostTarget :: c (a b h t m),
    depsTargetTarget :: c (a b t t m)
  }

deriving instance
  (CA Eq a m, (forall e. (Eq e) => Eq (c e))) =>
  Eq (SimpleDeps c a b h t m)

deriving instance
  (CA Show a m, (forall e. (Show e) => Show (c e))) =>
  Show (SimpleDeps c a b h t m)

instance
  (forall b1 h1 t1. Semigroup (c (a b1 h1 t1 m))) =>
  Semigroup (SimpleDeps c a b h t m)
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
  (forall b1 h1 t1. Monoid (c (a b1 h1 t1 m))) =>
  Monoid (SimpleDeps c a b h t m)
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
  propagatedDep :: a b h t m -> SimpleDeps c a b h t m
