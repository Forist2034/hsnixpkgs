{-# LANGUAGE TupleSections #-}

module HsNixPkgs.HsBuilder.DepStr
  ( DepStr (..),
    DepModM,
    mkKVMod,
  )
where

import Control.Applicative
import Data.Foldable
import Data.String
import HsNix.Derivation
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH.Syntax

newtype DepStr m = DepStr (m (DrvStr m))

instance (ApplicativeDeriv m) => IsString (DepStr m) where
  fromString = DepStr . pure . fromString

instance (ApplicativeDeriv m) => Semigroup (DepStr m) where
  DepStr l <> DepStr r = DepStr (liftA2 (<>) l r)

instance (ApplicativeDeriv m) => Monoid (DepStr m) where
  mempty = DepStr (pure mempty)

type DepModM mt m = ModuleM (DepStr m) mt

mkKVMod ::
  (ApplicativeDeriv m, Traversable t) =>
  String ->
  [String] ->
  [Extension] ->
  t (String, DrvStrBuilder m, DepStr m) ->
  HsModule (DepStr m) 'OtherModule (t Name)
mkKVMod modName d es kvs =
  stringModule
    modName
    ( DepStr
        ( toDrvStr
            . foldl'
              ( \i (n, t, v) ->
                  let sn = fromString n
                   in i
                        <> (sn <> " :: " <> t <> "\n")
                        <> (sn <> " = \"" <> fromDrvStr v <> "\"\n")
              )
              ( foldMap (\e -> "{-# LANGUAGE " <> fromString (show e) <> " #-}\n") es
                  <> ("module " <> fromString modName <> " where\n")
              )
            <$> traverse (\(n, t, DepStr v) -> (n,t,) <$> v) kvs
        )
    )
    d
    ( fmap
        (\(n, _, _) -> Name (OccName n) (NameQ (ModName modName)))
        kvs
    )
