{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Fixup.Strip (StripCfg (..), stripDirs) where

import Data.Default
import Data.Singletons
import qualified Data.Text as T
import qualified HsNixPkgs.Boot.Build.Fixup.Strip as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.HsBuilder.Generate
import HsNixPkgs.System
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data StripCfg (s :: System) = StripCfg
  { stripSubDirs :: [FilePath],
    stripFlags :: [Code HsQ String]
  }

instance Default (StripCfg s) where
  def =
    StripCfg
      { stripSubDirs = ["lib", "lib32", "lib64", "libexec", "bin", "sbin"],
        stripFlags = [liftTyped "--strip-debug"]
      }

stripDirs :: forall s. SingI s => StripCfg s -> Code HsQ (FilePath -> BIO ())
stripDirs sa =
  unsafeCodeCoerce
    [|
      B.stripDirs
        $( lift
             ( let prefix = toConfigTriple (fromSing (sing @s)) <> "-"
                in B.StripCfg
                     { B.stripCmd = T.unpack (prefix <> "strip"),
                       B.ranlibCmd = T.unpack (prefix <> "ranlib")
                     }
             )
         )
        $(listE (fmap unTypeCode (stripFlags sa)))
        $(lift (stripSubDirs sa))
      |]
