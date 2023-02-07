{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.Util
  ( unwordsDSB,
    unlinesDSB,
    escapeArgs,
  )
where

import Data.Char (isSpace)
import Data.Foldable
import Data.String
import HsNix.Derivation

escapeArg :: ApplicativeDeriv m => DrvStr m -> DrvStrBuilder m
escapeArg ds =
  foldMap
    ( \case
        QStr s -> fromDrvStr s
        QEscape c -> fromString ['\\', c]
    )
    (quote (\c -> isSpace c || c == '\'' || c == '\"' || c == '\\') ds)

unwordsDSB :: ApplicativeDeriv m => [DrvStrBuilder m] -> DrvStrBuilder m
unwordsDSB [] = mempty
unwordsDSB [x] = x
unwordsDSB (x : xs) = foldl' (\r i -> r <> " " <> i) x xs

unlinesDSB :: ApplicativeDeriv m => [DrvStrBuilder m] -> DrvStrBuilder m
unlinesDSB [] = mempty
unlinesDSB [x] = x
unlinesDSB (x : xs) = foldl' (\r i -> r <> "\n" <> i) x xs

escapeArgs :: ApplicativeDeriv m => [DrvStr m] -> DrvStrBuilder m
escapeArgs = unwordsDSB . fmap escapeArg
