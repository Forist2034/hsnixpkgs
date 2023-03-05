{-# LANGUAGE RankNTypes #-}

module HsNixPkgs.StdEnv.MakeDeriv (
  BuildEnv (..),
  emptyBuildEnv,
  BuilderCfg (..),
  BuilderArg (..),
  DerivType (..),
  MkDerivationArg (..),
  outputOpts,
  defMkDerivationArg,
  mkDerivation,
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Singletons
import Data.String
import Data.Text (Text)
import HsNix.Derivation
import qualified HsNix.Derivation as ND
import HsNix.DrvStr (DrvStr)
import HsNix.Hash
import HsNix.OutputName
import HsNix.StorePathName
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.ExtendDrv
import HsNixPkgs.StdEnv.MakeDeriv.CompileBuilder
import HsNixPkgs.StdEnv.StdEnv
import HsNixPkgs.System
import Language.Haskell.GenPackage
import Language.Haskell.TH (Code, Extension)

data BuildEnv = BuildEnv
  { beEnv :: HM.HashMap Text DrvStr,
    bePassAsFile :: HM.HashMap Text DrvStr
  }

emptyBuildEnv :: BuildEnv
emptyBuildEnv = BuildEnv {beEnv = HM.empty, bePassAsFile = HM.empty}

data BuilderArg b = BuilderArg
  { buildEnv :: DrvM BuildEnv,
    buildCfg :: BuilderCfg b,
    builderExt :: [Extension],
    builderMain :: ModuleM 'MainModule (Code HsQ (BIO ()))
  }

data MkDerivationArg a = MkDrvArg
  { pName :: Text,
    version :: Maybe Text,
    sourceDateEpoch :: Int,
    sslCertFile :: Maybe (DrvM DrvStr),
    derivType :: DerivType a,
    extraEnv :: DrvM BuildEnv
  }

defMkDerivationArg ::
  Text ->
  Maybe Text ->
  DerivType a ->
  MkDerivationArg a
defMkDerivationArg n v t =
  MkDrvArg
    { pName = n,
      version = v,
      sourceDateEpoch = 315532800, -- 1980-01-01 12:00:00
      sslCertFile = Nothing,
      derivType = t,
      extraEnv = pure emptyBuildEnv
    }

outputOpts :: MkDerivationArg a -> NEL.NonEmpty OutputName
outputOpts MkDrvArg {derivType = t} = case t of
  InputAddressed os -> os
  FixedOutput {} -> NEL.singleton fodOutputName
  ContentAddressed _ os -> os

mkDerivation ::
  forall a b h t s.
  (SingI b, SingI h, NamedHashAlgo a) =>
  StdEnv s b h t ->
  MkDerivationArg a ->
  BuilderArg b ->
  ExtendDeriv h t
mkDerivation std mda ba =
  let drv = ND.derivation $ do
        let buildPlatform = fromSing (sing @b)
        let name =
              let hp = fromSing (sing @h)
               in mconcat
                    [ pName mda,
                      if isStatic hp then "-static" else mempty,
                      if buildPlatform /= hp
                        then "-" <> toConfigTriple hp
                        else mempty,
                      maybe mempty ("-" <>) (version mda)
                    ]
        builder <-
          ND.drvStorePathStrOf
            ( compileBuilder
                name
                std
                (buildCfg ba)
                (builderExt ba)
                (builderMain ba)
            )
            Nothing
        sslCert <- fromMaybe (pure "/no-cert-file.crt") (sslCertFile mda)
        exEnv <- extraEnv mda
        bEnv <- buildEnv ba
        pure
          ( (ND.defaultDrvArg (makeStorePathNameThrow name) builder (toNixSys buildPlatform))
              { ND.drvType = derivType mda,
                ND.drvEnv =
                  ("TZ", "UTC")
                    : ("SOURCE_DATE_EPOCH", fromString (show (sourceDateEpoch mda)))
                    : ("NIX_SSL_CERT_FILE", sslCert)
                    : ("SSL_CERT_FILE", sslCert)
                    : HM.toList (beEnv bEnv)
                    ++ HM.toList (beEnv exEnv),
                ND.drvPassAsFile =
                  HM.toList (bePassAsFile bEnv)
                    ++ HM.toList (bePassAsFile exEnv)
              }
          )
   in ExtDrv
        { eDrvBaseDrv = drv,
          eDrvOutputs =
            fmap
              ( \n ->
                  DrvOutput
                    { dOutParentDrv = drv,
                      dOutName = Just n
                    }
              )
              (outputOpts mda)
        }