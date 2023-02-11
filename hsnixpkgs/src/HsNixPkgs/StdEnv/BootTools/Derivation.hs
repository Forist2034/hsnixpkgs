{-# LANGUAGE DeriveLift #-}

module HsNixPkgs.StdEnv.BootTools.Derivation
  ( UnpackedDeriv (..),
    UnpackDerivArg (..),
    unpackDeriv,
  )
where

import Data.String
import Data.Text (Text)
import HsNix.Derivation
import HsNix.Hash
import HsNix.System
import HsNixPkgs.Util
import Language.Haskell.TH.Syntax (Lift)

data UnpackedDeriv m = UnpackedDeriv
  { btName :: Text,
    btOriginalPath :: Text,
    btDerivation :: Derivation m
  }

data UnpackDerivArg = UnpackDerivArg
  { udaName :: Text,
    udaSystem :: System,
    udaStoreName :: Text,
    udaOrigStorePath :: Text
  }
  deriving (Show, Lift)

unpackDeriv ::
  ApplicativeDeriv m =>
  Derivation m ->
  UnpackDerivArg ->
  Derivation m ->
  [UnpackedDeriv m] ->
  UnpackedDeriv m
unpackDeriv unpackExec uda packedTool depBt =
  UnpackedDeriv
    { btName = udaName uda,
      btOriginalPath = udaOrigStorePath uda,
      btDerivation =
        derivation
          ( do
              builder <- storePathStr unpackExec
              package <- storePathStr packedTool
              rewrite <-
                traverse
                  ( \bt ->
                      fmap
                        ( \sp ->
                            unwordsDSB
                              [ fromDrvStr (toDrvStr (btName bt)),
                                fromDrvStr (toDrvStr (btOriginalPath bt)),
                                fromDrvStr sp
                              ]
                        )
                        (storePathStr (btDerivation bt))
                  )
                  depBt
              pure
                ( (defaultDrvArg @SHA256 (udaStoreName uda) builder (udaSystem uda))
                    { drvArgs =
                        [ package,
                          "rewritePath",
                          "out"
                        ],
                      drvPassAsFile =
                        [ ( "rewrite",
                            toDrvStr
                              ( unlinesDSB
                                  ( "1"
                                      : "out " <> fromDrvStr (toDrvStr (udaOrigStorePath uda))
                                      : fromString (show (length depBt))
                                      : rewrite
                                  )
                              )
                          )
                        ]
                    }
                )
          )
    }