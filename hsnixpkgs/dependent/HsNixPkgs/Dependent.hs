{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HsNixPkgs.Dependent (
  SimpleDeps (..),
  HasPropagatedDep (..),
  propagateDependencies,
  collectDepsA,
) where

import Data.Hashable
import Data.Singletons
import HsNixPkgs.Dependent.TH
import HsNixPkgs.Dependent.Types

genProgFunc

collectDepsA ::
  (Applicative m, SingI b, SingI h, HasPropagatedDep a c, CA Hashable a, Monoid v) =>
  SimpleDeps c a b h t ->
  [(SimpleDeps c a b h t, m v)] ->
  (SimpleDeps [] a b h t, m v)
collectDepsA d dl =
  let (ds, vs) = unzip dl
   in (propagateDependencies (d : ds), mconcat <$> sequenceA vs)