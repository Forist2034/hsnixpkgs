{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.Move
  ( moveLib64,
    moveSbin,
    moveSystemdUserUnits,
  )
where

import qualified HsNixPkgs.Boot.Build.Fixup.Move as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH

moveLib64 :: Code HsQ (FilePath -> BIO ())
moveLib64 = [||B.moveLib64||]

moveSbin :: Code HsQ (FilePath -> BIO ())
moveSbin = [||B.moveSbin||]

moveSystemdUserUnits :: Code HsQ (FilePath -> BIO ())
moveSystemdUserUnits = [||B.moveSystemdUserUnits||]