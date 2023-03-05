{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.Util (
  unwordsDSB,
  unlinesDSB,
  escapeArgs,
) where

import Data.Char (isSpace)
import Data.Foldable
import Data.String
import HsNix.DrvStr (DrvStr)
import qualified HsNix.DrvStr as DS
import qualified HsNix.DrvStr.Builder as DSB

escapeArg :: DrvStr -> DSB.Builder
escapeArg ds =
  foldMap
    ( \case
        DS.QStr s -> DSB.fromDrvStr s
        DS.QChar c -> fromString ['\\', c]
    )
    (DS.quote (\c -> isSpace c || c == '\'' || c == '\"' || c == '\\') ds)

unwordsDSB :: [DSB.Builder] -> DSB.Builder
unwordsDSB [] = mempty
unwordsDSB [x] = x
unwordsDSB (x : xs) = foldl' (\r i -> r <> " " <> i) x xs

unlinesDSB :: [DSB.Builder] -> DSB.Builder
unlinesDSB [] = mempty
unlinesDSB [x] = x
unlinesDSB (x : xs) = foldl' (\r i -> r <> "\n" <> i) x xs

escapeArgs :: [DrvStr] -> DSB.Builder
escapeArgs = unwordsDSB . fmap escapeArg
