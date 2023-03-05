module HsNixPkgs.StdEnv.BootTools.Derivation (
  UnpackedDeriv (..),
  unpackDeriv,
) where

import Data.Text (Text)
import HsNix.Derivation
import qualified HsNix.DrvStr.Builder as DSB
import HsNix.Hash
import HsNix.StorePathName
import HsNixPkgs.StdEnv.BootTools.UnpackArg
import HsNixPkgs.Util

data UnpackedDeriv = UnpackedDeriv
  { btName :: Text,
    btOriginalPath :: Text,
    btDerivation :: Derivation
  }

unpackDeriv ::
  Derivation ->
  UnpackDerivArg ->
  Derivation ->
  [UnpackedDeriv] ->
  UnpackedDeriv
unpackDeriv unpackExec uda packedTool depBt =
  UnpackedDeriv
    { btName = udaName uda,
      btOriginalPath = udaOrigStorePath uda,
      btDerivation =
        derivation @SHA256
          ( do
              builder <- drvStorePathStrOf unpackExec Nothing
              package <- drvStorePathStrOf packedTool Nothing
              rewrite <-
                traverse
                  ( \bt ->
                      fmap
                        ( \sp ->
                            unwordsDSB
                              [ DSB.fromText (btName bt),
                                DSB.fromText (btOriginalPath bt),
                                DSB.fromDrvStr sp
                              ]
                        )
                        (drvStorePathStrOf (btDerivation bt) Nothing)
                  )
                  depBt
              pure
                ( ( defaultDrvArg
                      (makeStorePathNameThrow (udaStoreName uda))
                      builder
                      (udaSystem uda)
                  )
                    { drvArgs =
                        [ package,
                          "rewritePath",
                          "out"
                        ],
                      drvPassAsFile =
                        [ ( "rewrite",
                            DSB.toDrvStr
                              ( unlinesDSB
                                  ( "1"
                                      : "out " <> DSB.fromText (udaOrigStorePath uda)
                                      : DSB.decimal (length depBt)
                                      : rewrite
                                  )
                              )
                          )
                        ]
                    }
                )
          )
    }