{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-redundant-constraints #-}

module HsNixPkgs.System.Cpu where

import Data.Singletons.Base.TH
import Data.Text (Text)

singletons
  [d|
    data BitWidth
      = Cpu8
      | Cpu16
      | Cpu32
      | Cpu64
      deriving (Eq, Show)

    data SignificantByte = BigEndian | LittleEndian
      deriving (Eq, Show)

    data X86Arch
      = Amd64Arch
      | I686Arch
      deriving (Eq, Show)

    data Cpu = X86 X86Arch
      deriving (Eq, Show)

    cpuBitWidth :: Cpu -> BitWidth
    cpuBitWidth (X86 I686Arch) = Cpu32
    cpuBitWidth (X86 Amd64Arch) = Cpu64

    cpuSignificantByte :: Cpu -> SignificantByte
    cpuSignificantByte (X86 {}) = LittleEndian

    i686, amd64 :: Cpu
    i686 = X86 I686Arch
    amd64 = X86 Amd64Arch
    |]

cpuName :: Cpu -> Text
cpuName (X86 I686Arch) = "i686"
cpuName (X86 Amd64Arch) = "x86_64"

-- | Determine when two CPUs are compatible with each other. That is,
--   can code built for system B run on system A? For that to happen,
--   the programs that system B accepts must be a subset of the
--   programs that system A accepts.
isCompatible :: Cpu -> Cpu -> Bool
isCompatible a (X86 I686Arch) = isCompatible a amd64
isCompatible a b = a == b