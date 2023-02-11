{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.StdEnv.Linux.SupportTools.Amd64 (unpackStdenvLinux, compileBuilder) where

import qualified HsNix.System as NS
import HsNixPkgs.StdEnv.BootTools.RequireFile
import HsNixPkgs.StdEnv.BootTools.TH

decodeSpec "stdenv/linux/amd64/support-tool.yaml"
  >>= mkSupportTools
    [|requireFile|]
    NS.x86_64_linux
