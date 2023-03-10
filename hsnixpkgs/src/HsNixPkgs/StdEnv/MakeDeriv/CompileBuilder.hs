{-# LANGUAGE TupleSections #-}

module HsNixPkgs.StdEnv.MakeDeriv.CompileBuilder (
  BuilderCfg (..),
  compileBuilder,
) where

import Control.Monad
import Data.Bifunctor
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NEL
import Data.Singletons
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import HsNix.Derivation
import HsNix.DrvStr (DrvStr)
import qualified HsNix.DrvStr as DS
import qualified HsNix.DrvStr.Builder as DSB
import HsNix.Hash
import HsNix.OutputName
import HsNix.StorePathName
import HsNixPkgs.Build.Main
import HsNixPkgs.Dependent
import HsNixPkgs.Develop.C.Library
import HsNixPkgs.Develop.Haskell.Package
import HsNixPkgs.Develop.NativeLibrary
import qualified HsNixPkgs.ExtendDrv as ED
import HsNixPkgs.HsBuilder.DepStr
import HsNixPkgs.SetupHook
import HsNixPkgs.StdEnv.StdEnv
import HsNixPkgs.System
import HsNixPkgs.Util
import Language.Haskell.GenPackage
import Language.Haskell.TH

data BuilderCfg b = BuilderCfg
  { builderDep :: [HsPackage StdEnvGhc b b b],
    builderFlags :: [DrvStr]
  }

instance Default (BuilderCfg b) where
  def =
    BuilderCfg
      { builderDep = [],
        builderFlags = ["-threaded"]
      }

coreLibrary :: HS.HashSet Text
coreLibrary =
  HS.fromList
    [ "Cabal",
      "Cabal-syntax",
      "array",
      "base",
      "binary",
      "bytestring",
      "containers",
      "deepseq",
      "directory",
      "exceptions",
      "filepath",
      "ghc-bignum",
      "ghc-boot",
      "ghc-boot-th",
      "ghc-compact",
      "ghc-heap",
      "ghc-prim",
      "ghci",
      "haskeline",
      "hpc",
      "integer-gmp",
      "libiserv",
      "mtl",
      "parsec",
      "pretty",
      "process",
      "stm",
      "template-haskell",
      "terminfo",
      "text",
      "time",
      "transformers",
      "unix",
      "xhtml"
    ]

procDep ::
  (SingI b, SingI h) =>
  [HsPackage hsc b h t] ->
  ( SimpleDeps [] (HsPackage hsc) b h t,
    SimpleDeps [] CLibrary b h t
  )
procDep d =
  let pd = propagateDependencies [mempty {depsHostTarget = HsPackageDep <$> d}]
      bb = group (depsBuildBuild pd)
      bh = group (depsBuildHost pd)
      bt = group (depsBuildTarget pd)
      hh = group (depsHostHost pd)
      ht = group (depsHostTarget pd)
      tt = group (depsTargetTarget pd)
   in ( SimpleDeps
          { depsBuildBuild = fst bb,
            depsBuildHost = fst bh,
            depsBuildTarget = fst bt,
            depsHostHost = fst hh,
            depsHostTarget = fst ht,
            depsTargetTarget = fst tt
          },
        SimpleDeps
          { depsBuildBuild = snd bb,
            depsBuildHost = snd bh,
            depsBuildTarget = snd bt,
            depsHostHost = snd hh,
            depsHostTarget = snd ht,
            depsTargetTarget = snd tt
          }
      )
  where
    group [] = ([], [])
    group (x : xs) =
      let (hs, c) = group xs
       in case x of
            HsPackageDep p -> (p : hs, c)
            HsFFIDep f -> (hs, f : c)

checkDep :: HS.HashSet Text -> SimpleDeps [] (HsPackage StdEnvGhc) b b b -> HS.HashSet Text
checkDep spec prov =
  spec
    `HS.difference` HS.union
      coreLibrary
      ( HS.fromList
          ( foldMap
              hsPkgs
              ( concat
                  [ depsHostHost prov,
                    depsHostTarget prov,
                    depsTargetTarget prov
                  ]
              )
          )
      )

compileBuilder ::
  forall b h t s.
  SingI b =>
  Text ->
  StdEnv s b h t ->
  BuilderCfg b ->
  [Extension] ->
  ModuleM 'MainModule (Code HsQ (BIO ())) ->
  Derivation
compileBuilder name env cfg ext main =
  let (extDep, mModEnvs) =
        let hsE = hsExecutable (mainModule ext (runBuildMain <$> main))
         in ( HS.map T.pack (execDependencies hsE),
              fmap
                ( first
                    ( \mn ->
                        let t = T.pack mn
                         in ( t, -- module name
                              T.replace "." "_" (T.replace "_" "_m" t) -- module env
                            )
                    )
                )
                (HM.toList (execModules hsE))
            )
      (hsD, cD) = procDep (builderLibBoot env : builderDep cfg)
   in derivation
        ( do
            let df = checkDep extDep hsD
             in unless (HS.null df) $
                  error
                    ( concat
                        [ "Compile builder ",
                          T.unpack name,
                          ": missing dependencies ",
                          show df
                        ]
                    )
            sh <- nativeLib mempty [cLibrary cD []]
            pkgConfs <-
              DSB.toDrvStr . unwordsDSB
                <$> traverse
                  (fmap DSB.fromDrvStr . packageConfDir)
                  ( concat
                      [ depsHostHost hsD,
                        depsHostTarget hsD,
                        depsTargetTarget hsD
                      ]
                  )
            gh <- ED.getStorePathStr (stdEnvGhc env)
            bb <- ED.getStorePathStr (compileBuilderBin env)
            modEnvs <- traverse (\((_, e), DepStr v) -> (e,) <$> v) mModEnvs
            pure
              ( ( defaultDrvArg
                    (makeStorePathNameThrow (name <> "-builder"))
                    bb
                    (toNixSys (fromSing (sing @b)))
                )
                  { drvEnv =
                      ("ghc", gh <> "/bin/ghc")
                        : ( "ghc_flags",
                            DSB.toDrvStr
                              ( escapeArgs
                                  ( foldr'
                                      (\p s -> "-package" : DS.fromText p : s)
                                      (builderFlags cfg)
                                      extDep
                                  )
                              )
                          )
                        : ("sources", fromString (show (fmap (second (<> "Path") . fst) mModEnvs)))
                        : ("packages", pkgConfs)
                        : HM.toList (newEnv sh),
                    drvPassAsFile = modEnvs ++ HM.toList (newPassAsFile sh),
                    drvType =
                      ContentAddressed
                        @SHA256
                        HashFlat
                        (NEL.singleton (makeOutputNameThrow "out"))
                  }
              )
        )
