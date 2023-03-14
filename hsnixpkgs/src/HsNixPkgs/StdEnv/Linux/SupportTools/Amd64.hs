{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.StdEnv.Linux.SupportTools.Amd64 (stdenvUnpackLinux, compileBuilder) where

import HsNix.Derivation
import qualified HsNix.System as NS
import HsNixPkgs.StdEnv.BootTools.RequireFile
import HsNixPkgs.StdEnv.BootTools.TH
import Language.Haskell.TH (unsafeCodeCoerce)

decodeSpec "stdenv/spec/linux/amd64/support-tool.yaml"
  >>= mkSupportTools
    ''Derivation
    (unsafeCodeCoerce [|requireFile|])
    NS.x86_64_linux
