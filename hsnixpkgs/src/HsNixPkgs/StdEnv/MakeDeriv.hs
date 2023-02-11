{-# LANGUAGE RankNTypes #-}

module HsNixPkgs.StdEnv.MakeDeriv
  ( BuildEnv (..),
    emptyBuildEnv,
    BuilderCfg (..),
    BuilderArg (..),
    DerivType (..),
    MkDerivationArg (..),
    defMkDerivationArg,
    outputOpts,
    mkDerivation,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Singletons
import Data.String
import Data.Text (Text)
import HsNix.Derivation (DerivType (..))
import qualified HsNix.Derivation as ND
import HsNix.Hash
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.ExtendDrv
import HsNixPkgs.HsBuilder.DepStr (DepStr)
import HsNixPkgs.HsBuilder.Generate
import HsNixPkgs.StdEnv.MakeDeriv.CompileBuilder
import HsNixPkgs.StdEnv.StdEnv
import HsNixPkgs.System
import Language.Haskell.TH (Code, Extension)

data BuildEnv m = BuildEnv
  { beEnv :: HM.HashMap Text (DrvStr m),
    bePassAsFile :: HM.HashMap Text (DrvStr m)
  }

emptyBuildEnv :: BuildEnv m
emptyBuildEnv = BuildEnv {beEnv = HM.empty, bePassAsFile = HM.empty}

data BuilderArg b m = BuilderArg
  { buildEnv :: m (BuildEnv m),
    buildCfg :: BuilderCfg b m,
    builderExt :: [Extension],
    builderMain :: ModuleM (DepStr m) 'MainModule (Code HsQ (BIO ()))
  }

data MkDerivationArg a m = MkDrvArg
  { pName :: Text,
    version :: Maybe Text,
    sourceDateEpoch :: Int,
    sslCertFile :: Maybe (m (DrvStr m)),
    derivType :: DerivType a,
    extraEnv :: m (BuildEnv m)
  }

defMkDerivationArg ::
  ApplicativeDeriv m =>
  Text ->
  Maybe Text ->
  DerivType a ->
  MkDerivationArg a m
defMkDerivationArg n v t =
  MkDrvArg
    { pName = n,
      version = v,
      sourceDateEpoch = 315532800, -- 1980-01-01 12:00:00
      sslCertFile = Nothing,
      derivType = t,
      extraEnv = pure emptyBuildEnv
    }

outputOpts :: MkDerivationArg a m -> NEL.NonEmpty Text
outputOpts MkDrvArg {derivType = t} = case t of
  InputAddressed os -> os
  FixedOutput _ _ -> NEL.singleton "out"
  ContentAddressed _ os -> os

mkDerivation ::
  forall a b h t s m.
  (SingI b, SingI h, ApplicativeDeriv m, NamedHashAlgo a) =>
  StdEnv s b h t m ->
  MkDerivationArg a m ->
  BuilderArg b m ->
  ExtendDeriv h t m
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
          ND.storePathStr
            ( compileBuilder
                name
                std
                (buildCfg ba)
                (builderExt ba)
                (builderMain ba)
            )
        sslCert <- fromMaybe (pure "/no-cert-file.crt") (sslCertFile mda)
        exEnv <- extraEnv mda
        bEnv <- buildEnv ba
        pure
          ( (ND.defaultDrvArg name builder (toNixSys buildPlatform))
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