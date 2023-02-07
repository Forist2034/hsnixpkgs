{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.Dependent
  ( SimpleDeps (..),
    HasPropagatedDep (..),
    propagateDependencies,
    collectDepsA,
  )
where

import Data.Hashable
import Data.Singletons
import HsNixPkgs.Dependent.TH
import HsNixPkgs.Dependent.Types

genProgFunc

collectDepsA ::
  (Applicative m, SingI b, SingI h, HasPropagatedDep a c, CA Eq a m, CA Hashable a m, Monoid v) =>
  SimpleDeps c a b h t m ->
  [(SimpleDeps c a b h t m, m v)] ->
  (SimpleDeps [] a b h t m, m v)
collectDepsA d dl =
  let (ds, vs) = unzip dl
   in (propagateDependencies (d : ds), mconcat <$> sequenceA vs)