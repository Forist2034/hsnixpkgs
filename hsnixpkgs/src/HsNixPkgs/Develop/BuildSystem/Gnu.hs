{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Develop.BuildSystem.Gnu (
  PhaseBuildInfo,
  SourceList,
  UnpackCfg (..),
  UnpackPhase (..),
  unpackExpr,
  mkUnpackPhase,
  unpackPhase,
  PatchCfg (..),
  Patch (..),
  patchExpr,
  mkPatchPhase,
  patchPhase,
  ConfigureCfg (..),
  ConfigurePhase (..),
  configureExpr,
  mkConfigurePhase,
  configurePhase,
  MakeCfg (..),
  BuildCfg (..),
  buildExpr,
  mkBuildPhase,
  buildPhase,
  CheckCfg (..),
  CheckPhase (..),
  checkExpr,
  mkCheckPhase,
  checkPhase,
  InstallCfg (..),
  InstallPhase (..),
  installExpr,
  mkInstallPhase,
  installPhase,
  SimpleFixupCfg (..),
  simpleFixupCfg,
  fixupExpr,
  mkFixupPhase,
  fixupPhase,
  InstallCheckCfg (..),
  InstallCheckPhase (..),
  installCheckExpr,
  mkInstallCheckPhase,
  installCheckPhase,
  DistCfg (..),
  DistPhase (..),
  distExpr,
  mkDistPhase,
  distPhase,
  GnuBuildPhases (..),
  simpleGnuBuildPhases,
  GnuBuildCfg (..),
  gnuBuildSystem,
) where

import Control.Applicative
import Data.Bifunctor
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Singletons
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import HsNix.Derivation
import HsNix.DrvStr (DrvStr)
import qualified HsNix.DrvStr.Builder as DSB
import HsNix.Hash (NamedHashAlgo)
import HsNix.OutputName
import qualified HsNixPkgs.Boot.Develop.BuildSystem.Gnu as B
import HsNixPkgs.Build.Fixup
import qualified HsNixPkgs.Build.Fixup.CompressManPages as F
import qualified HsNixPkgs.Build.Fixup.MakeSymlinkRelative as F
import qualified HsNixPkgs.Build.Fixup.Move as F
import qualified HsNixPkgs.Build.Fixup.MoveDocs as F
import qualified HsNixPkgs.Build.Fixup.MultiOutput as F
import qualified HsNixPkgs.Build.Fixup.PatchShebang as F
import qualified HsNixPkgs.Build.Fixup.PruneLibtoolFiles as F
import qualified HsNixPkgs.Build.Fixup.Strip as F
import HsNixPkgs.Build.Hook
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.Build.MultiOutput
import HsNixPkgs.Build.Patch
import HsNixPkgs.Build.Phase
import HsNixPkgs.Build.Unpack
import HsNixPkgs.Dependent
import HsNixPkgs.Develop.NativeLibrary
import HsNixPkgs.ExtendDrv
import HsNixPkgs.HsBuilder.DepStr
import HsNixPkgs.HsBuilder.OutputMod
import HsNixPkgs.HsBuilder.Util
import HsNixPkgs.SetupHook
import HsNixPkgs.SetupHook.Executable
import HsNixPkgs.StdEnv.MakeDeriv
import HsNixPkgs.StdEnv.StdEnv
import HsNixPkgs.System
import HsNixPkgs.Util
import Language.Haskell.GenPackage
import Language.Haskell.TH hiding (Phases)
import Language.Haskell.TH.Syntax hiding (Phases)

type PhaseM = ModuleM 'MainModule (Code HsQ Phase)

type ModExprM = ModuleM 'MainModule (Code HsQ (BIO ()))

type MkPhase = Code HsQ (BIO ()) -> HsQ [Dec] -> PhaseM

mkDefPhDecl :: ModExprM -> MkPhase -> PhaseM
mkDefPhDecl me f = do
  e <- me
  f e (pure [])

data PhaseBuildInfo = PhaseBuildInfo
  { multiOutputArg :: Maybe (MultiOutput OutputName, MultiOutput (Code HsQ FilePath)),
    nativeLibSearchDir :: DrvM [DrvStr],
    pathEnv :: DrvM ([DSB.Builder], [DSB.Builder], [DSB.Builder]),
    outputs :: HM.HashMap OutputName (Code HsQ FilePath)
  }

data UnpackPhase = UnpackPhase
  { unpackSources :: SourceList,
    unpackSourceRoot :: FilePath,
    unpackCfg :: UnpackCfg
  }

data ConfigureP

data ConfigureCfg (h :: System) (t :: System) = ConfigureCfg
  { configureScript :: String,
    configureFlags :: [Code HsQ String],
    prefixKey :: String,
    addDisableDepTrack :: Bool,
    fixLibtool :: Bool,
    disableStatic :: Bool,
    configurePlatform :: [(String, String)],
    configureHook :: Hook ConfigureP
  }

instance (SingI h, SingI t) => Default (ConfigureCfg h t) where
  def =
    ConfigureCfg
      { configureScript = "configure",
        configureFlags = [],
        prefixKey = "--prefix",
        addDisableDepTrack = True,
        fixLibtool = True,
        disableStatic = True,
        configurePlatform =
          [ ("host", T.unpack (toConfigTriple (fromSing (sing @h)))),
            ("target", T.unpack (toConfigTriple (fromSing (sing @t))))
          ],
        configureHook = def
      }

data ConfigurePhase h t = ConfigurePhase
  { configPrefix :: Maybe (Code HsQ FilePath),
    configureCfg :: ConfigureCfg h t
  }

mkConfigurePhase :: MkPhase
mkConfigurePhase = newPhase "configurePhase" "configuring"

configureExpr ::
  PhaseBuildInfo ->
  ConfigurePhase h t ->
  ModExprM
configureExpr pbi cp@ConfigurePhase {configureCfg = cfg} = do
  msp <-
    if fixLibtool cfg
      then
        (\v -> [||Just $$(unsafeCodeCoerce (varE v))||])
          <$> useModule libSearchMod
      else pure [||Nothing||]
  pure
    ( runHook
        (configureHook cfg)
        [||
        B.configureFunc
          $$(liftTyped (configureScript cfg))
          $$configFlags
          $$msp
          $$( case configPrefix cp of
                Just p -> [||Just $$p||]
                Nothing -> [||Nothing||]
            )
        ||]
    )
  where
    libSearchMod =
      stringModule
        "Configure.LibSearchPath"
        StringModule
          { smText =
              DepStr
                ( fmap
                    ( \s ->
                        DSB.toDrvStr
                          ( unlinesDSB
                              [ "{-# LANGUAGE OverloadedStrings #-}",
                                "module Configure.LibSearchPath where",
                                mempty,
                                "libSearchPath :: Text",
                                "libSearchPath = \""
                                  <> unwordsDSB (fmap DSB.fromDrvStr s)
                                  <> "\""
                              ]
                          )
                    )
                    (nativeLibSearchDir pbi)
                ),
            smExtDep = ["base", "text"],
            smValue =
              Name
                (OccName "libSearchPath")
                (NameQ (ModName "Configure.LibSearchPath"))
          }

    configFlags :: Code HsQ [String]
    configFlags =
      listET
        ( concat
            [ case configPrefix cp of
                Just p ->
                  [ [||
                    $$(liftTyped (concat ["--", prefixKey cfg, "="]))
                      ++ $$p
                    ||]
                  ]
                Nothing -> [],
              fmap
                (\(n, v) -> liftTyped (concat ["--", n, "=", v]))
                (configurePlatform cfg),
              case multiOutputArg pbi of
                Just (_, moa) ->
                  [ [||"--bindir=" ++ $$(outBin moa) ++ "/bin"||],
                    [||"--sbindir=" ++ $$(outBin moa) ++ "/sbin"||],
                    [||"--includedir=" ++ $$(outInclude moa) ++ "/include"||],
                    [||"--oldincludedir=" ++ $$(outInclude moa) ++ "/include"||],
                    [||"--mandir=" ++ $$(outMan moa) ++ "/share/man"||],
                    [||"--infodir=" ++ $$(outInfo moa) ++ "/share/info"||],
                    [||
                    "--docdir="
                      ++ $$(outDoc moa)
                      ++ "/share/doc/"
                      ++ $$(liftTyped (T.unpack (shareDocName moa)))
                    ||],
                    [||"--libdir=" ++ $$(outLib moa) ++ "/lib"||],
                    [||"--libexecdir=" ++ $$(outLib moa) ++ "/libexec"||],
                    [||"--localedir=" ++ $$(outLib moa) ++ "/share/locale"||]
                  ]
                Nothing -> [],
              configureFlags cfg
            ]
        )

configurePhase ::
  PhaseBuildInfo ->
  ConfigurePhase h t ->
  PhaseM
configurePhase pbi cp = mkDefPhDecl (configureExpr pbi cp) mkConfigurePhase

newtype MakeCfg = MakeCfg {makeFile :: String}

instance Default MakeCfg where
  def = MakeCfg {makeFile = "Makefile"}

data BuildP

data BuildCfg = BuildCfg
  { buildFlags :: [Code HsQ String],
    buildInParallel :: Bool,
    buildHook :: Hook BuildP
  }

instance Default BuildCfg where
  def =
    BuildCfg
      { buildFlags = [],
        buildInParallel = True,
        buildHook = def
      }

mkBuildPhase :: MkPhase
mkBuildPhase = newPhase "buildPhase" "building"

buildExpr :: MakeCfg -> BuildCfg -> Code HsQ (BIO ())
buildExpr mc cfg =
  runHook
    (buildHook cfg)
    [||
    B.runMake
      "build"
      $$(listET (buildFlags cfg))
      $$( liftTyped
            B.MakeCfg
              { B.mcMakeFile = makeFile mc,
                B.mcEnableParallel = buildInParallel cfg
              }
        )
    ||]

buildPhase :: MakeCfg -> BuildCfg -> PhaseM
buildPhase mc cfg = mkBuildPhase (buildExpr mc cfg) (pure [])

data CheckP

data CheckCfg = CheckCfg
  { checkInParallel :: Bool,
    checkFlags :: [Code HsQ String],
    checkHook :: Hook CheckP
  }

instance Default CheckCfg where
  def =
    CheckCfg
      { checkInParallel = True,
        checkFlags = [],
        checkHook = def
      }

data CheckPhase = CheckPhase
  { checkTarget :: [String],
    checkCfg :: CheckCfg
  }

mkCheckPhase :: MkPhase
mkCheckPhase = newPhase "checkPhase" "running tests"

checkExpr :: MakeCfg -> CheckPhase -> Code HsQ (BIO ())
checkExpr mc cp@CheckPhase {checkCfg = cfg} =
  runHook
    (checkHook cfg)
    [||
    B.runMake
      "check"
      $$( listET
            ( checkFlags cfg
                ++ fmap liftTyped (checkTarget cp)
            )
        )
      $$( liftTyped
            B.MakeCfg
              { B.mcMakeFile = makeFile mc,
                B.mcEnableParallel = checkInParallel cfg
              }
        )
    ||]

checkPhase :: MakeCfg -> CheckPhase -> PhaseM
checkPhase mc cp = mkCheckPhase (checkExpr mc cp) (pure [])

data InstallP

data InstallCfg = InstallCfg
  { installFlags :: [Code HsQ String],
    installHook :: Hook InstallP
  }

instance Default InstallCfg where
  def =
    InstallCfg
      { installFlags = [],
        installHook = def
      }

data InstallPhase = InstallPhase
  { installDest :: [Code HsQ FilePath],
    installTarget :: [String],
    installCfg :: InstallCfg
  }

mkInstallPhase :: MkPhase
mkInstallPhase = newPhase "installPhase" "install"

installExpr :: PhaseBuildInfo -> MakeCfg -> InstallPhase -> Code HsQ (BIO ())
installExpr pbi mc ip@InstallPhase {installCfg = cfg} =
  runHook
    (installHook cfg)
    [||
    B.installFunc
      $$(listET (installDest ip))
      $$( listET
            ( concat
                [ case multiOutputArg pbi of
                    Just (_, moa) ->
                      let dev = outDev moa
                       in [ [||"pkgconfigdir=" ++ $$dev ++ "/lib/pkgconfig"||],
                            [||"m4datadir=" ++ $$dev ++ "/share/aclocal"||],
                            [||"aclocaldir=" ++ $$dev ++ "/share/aclocal"||]
                          ]
                    Nothing -> [],
                  installFlags cfg,
                  fmap liftTyped (installTarget ip)
                ]
            )
        )
      $$( liftTyped
            B.MakeCfg
              { B.mcMakeFile = makeFile mc,
                B.mcEnableParallel = False
              }
        )
    ||]

installPhase ::
  PhaseBuildInfo ->
  MakeCfg ->
  InstallPhase ->
  PhaseM
installPhase pbi mc ip = mkInstallPhase (installExpr pbi mc ip) (pure [])

data SimpleFixupCfg h t = SimpleFixupCfg
  { forceShare :: Maybe F.ForceShare,
    rewriteSymLink :: Bool,
    gzipMan :: Bool,
    stripArgHost :: Maybe (F.StripCfg h),
    stripArgTarget :: Maybe (F.StripCfg t),
    patchSheBang :: Bool,
    pruneLibtoolFiles :: Bool,
    moveSystemdUserUnits :: Bool,
    moveSbin :: Bool,
    moveLib64 :: Bool
  }

instance Default (SimpleFixupCfg h t) where
  def =
    SimpleFixupCfg
      { forceShare = Just def,
        rewriteSymLink = True,
        gzipMan = True,
        stripArgHost = Just def,
        stripArgTarget = Nothing,
        patchSheBang = True,
        pruneLibtoolFiles = True,
        moveSystemdUserUnits = True,
        moveSbin = True,
        moveLib64 = True
      }

simpleFixupCfg ::
  (SingI h, SingI t) =>
  PhaseBuildInfo ->
  SimpleFixupCfg h t ->
  ModuleM 'MainModule [FixupTarget]
simpleFixupCfg pbi cfg = do
  patchSheBangF <-
    if patchSheBang cfg
      then do
        let toCode = unsafeCodeCoerce . varE
        (path, host_path) <- bimap toCode toCode <$> useModule pathsMod
        pure
          ( \name ->
              Just
                ( F.patchSheBang
                    ( case multiOutputArg pbi of
                        Just (mos, _)
                          {-Dev output will end up being run on the build platform. An
                          example case of this is sdl2-config. Otherwise, we can just
                          use the runtime path (--host).-}
                          | name == outDev mos -> path
                          | otherwise -> host_path
                        Nothing -> host_path
                    )
                )
          )
      else pure (const Nothing)
  pure
    ( fmap
        ( \(name, out_path) ->
            FixupTarget
              { ftPath = out_path,
                ftFixupFunc =
                  catMaybes
                    [ F.moveToShare <$> forceShare cfg,
                      [F.makeSymlinkRelative | rewriteSymLink cfg],
                      [F.compressManPages | gzipMan cfg],
                      F.stripDirs <$> stripArgHost cfg,
                      F.stripDirs <$> stripArgTarget cfg,
                      patchSheBangF name,
                      [F.pruneLibtoolFiles | pruneLibtoolFiles cfg],
                      F.multiOutputFixup . snd <$> multiOutputArg pbi,
                      [F.moveSystemdUserUnits | moveSystemdUserUnits cfg],
                      [F.moveSbin | moveSbin cfg],
                      [F.moveLib64 | moveLib64 cfg]
                    ]
              }
        )
        (HM.toList (outputs pbi))
    )
  where
    pathsMod =
      let modName = "Fixup.PatchSheBang.Path"
       in stringModule
            modName
            StringModule
              { smText =
                  DepStr
                    ( fmap
                        ( \(pb, ph, _) ->
                            DSB.toDrvStr
                              ( unlinesDSB
                                  [ "module " <> fromString modName <> " where",
                                    mempty,
                                    "path :: [FilePath]",
                                    "path = " <> toDSB pb,
                                    mempty,
                                    "hostPath :: [FilePath]",
                                    "hostPath = " <> toDSB ph
                                  ]
                              )
                        )
                        (pathEnv pbi)
                    ),
                smExtDep = [],
                smValue =
                  ( Name (OccName "path") (NameQ (ModName modName)),
                    Name (OccName "hostPath") (NameQ (ModName modName))
                  )
              }
    quoteDSB x = "\"" <> x <> "\""
    toDSB [] = mempty
    toDSB [x] = "[" <> quoteDSB x <> "]"
    toDSB (x : xs) =
      foldl'
        (\i r -> i <> "," <> quoteDSB r)
        ("[" <> quoteDSB x)
        xs

data InstallCheckP

data InstallCheckCfg = InstallCheckCfg
  { installCheckFlags :: [Code HsQ String],
    installCheckInParallel :: Bool,
    installCheckHook :: Hook InstallCheckP
  }

instance Default InstallCheckCfg where
  def =
    InstallCheckCfg
      { installCheckFlags = [],
        installCheckInParallel = False,
        installCheckHook = def
      }

data InstallCheckPhase = InstallCheckPhase
  { installCheckTargets :: [String],
    installCheckCfg :: InstallCheckCfg
  }

mkInstallCheckPhase :: MkPhase
mkInstallCheckPhase = newPhase "installCheckPhase" "running install tests"

installCheckExpr :: MakeCfg -> InstallCheckPhase -> Code HsQ (BIO ())
installCheckExpr mc icp@InstallCheckPhase {installCheckCfg = cfg} =
  runHook
    (installCheckHook cfg)
    [||
    B.runMake
      "install check"
      $$( listET
            ( installCheckFlags cfg
                ++ fmap liftTyped (installCheckTargets icp)
            )
        )
      $$( liftTyped
            B.MakeCfg
              { B.mcMakeFile = makeFile mc,
                B.mcEnableParallel = installCheckInParallel cfg
              }
        )
    ||]

installCheckPhase :: MakeCfg -> InstallCheckPhase -> PhaseM
installCheckPhase mc ip = mkInstallCheckPhase (installCheckExpr mc ip) (pure [])

data DistP

data DistCfg = DistCfg
  { distFlags :: [Code HsQ String],
    distHook :: Hook DistP
  }

instance Default DistCfg where
  def =
    DistCfg
      { distFlags = [],
        distHook = def
      }

data DistPhase = DistPhase
  { distTargets :: [String],
    distTarballs :: Maybe (Code HsQ FilePath, [FilePath]),
    distCfg :: DistCfg
  }

mkDistPhase :: MkPhase
mkDistPhase = newPhase "distPhase" "distPhase"

distExpr :: MakeCfg -> DistPhase -> Code HsQ (BIO ())
distExpr mc dp@DistPhase {distCfg = cfg} =
  runHook
    (distHook cfg)
    [||
    B.distFunc
      $$( listET
            ( distFlags cfg
                ++ fmap liftTyped (distTargets dp)
            )
        )
      $$( liftTyped
            B.MakeCfg
              { B.mcMakeFile = makeFile mc,
                B.mcEnableParallel = False
              }
        )
      $$( case distTarballs dp of
            Just (d, t) -> [||Just ($$d, $$(liftTyped t))||]
            Nothing -> [||Nothing||]
        )
    ||]

distPhase :: MakeCfg -> DistPhase -> PhaseM
distPhase mc dp = mkDistPhase (distExpr mc dp) (pure [])

type Phases = [PhaseM]

type DefPhase a = Either Phases a

data GnuBuildPhases h t = GnuBuildPhases
  { prePhases :: Phases,
    unpackArg :: DefPhase UnpackPhase,
    patchArg :: DefPhase (NEL.NonEmpty Patch, PatchCfg),
    preConfigurePhases :: Phases,
    configureArg :: DefPhase (ConfigurePhase h t),
    preBuildPhases :: Phases,
    buildArg :: DefPhase BuildCfg,
    checkArg :: DefPhase CheckPhase,
    preInstallPhases :: Phases,
    installArg :: DefPhase InstallPhase,
    fixupArg :: DefPhase (Hook FixupP, SimpleFixupCfg h t),
    installCheckArg :: DefPhase InstallCheckPhase,
    preDistPhases :: Phases,
    distArg :: DefPhase DistPhase,
    postPhases :: Phases
  }

simpleGnuBuildPhases ::
  DefPhase UnpackPhase ->
  DefPhase (ConfigurePhase h t) ->
  DefPhase InstallPhase ->
  GnuBuildPhases h t
simpleGnuBuildPhases u c i =
  GnuBuildPhases
    { prePhases = [],
      unpackArg = u,
      patchArg = Left [],
      preConfigurePhases = [],
      configureArg = c,
      preBuildPhases = [],
      buildArg = Right def,
      checkArg = Left [],
      preInstallPhases = [],
      installArg = i,
      fixupArg = Right (def, def),
      installCheckArg = Left [],
      preDistPhases = [],
      distArg = Left [],
      postPhases = []
    }

toPhases :: (a -> PhaseM) -> DefPhase a -> Phases
toPhases f = either id (L.singleton . f)

buildPhases ::
  (SingI h, SingI t) =>
  PhaseBuildInfo ->
  MakeCfg ->
  GnuBuildPhases h t ->
  ModuleM 'MainModule (Code HsQ (BIO ()))
buildPhases pbi ma bp =
  runPhases
    <$> sequence
      ( concat
          [ prePhases bp,
            toPhases
              ( \u ->
                  unpackPhase
                    (unpackSources u)
                    (unpackSourceRoot u)
                    (unpackCfg u)
              )
              (unpackArg bp),
            toPhases (uncurry patchPhase) (patchArg bp),
            preConfigurePhases bp,
            toPhases (configurePhase pbi) (configureArg bp),
            preBuildPhases bp,
            toPhases (buildPhase ma) (buildArg bp),
            toPhases (checkPhase ma) (checkArg bp),
            preInstallPhases bp,
            toPhases (installPhase pbi ma) (installArg bp),
            toPhases
              ( \(h, sfc) ->
                  simpleFixupCfg pbi sfc
                    >>= fixupPhase h
              )
              (fixupArg bp),
            toPhases (installCheckPhase ma) (installCheckArg bp),
            preDistPhases bp,
            toPhases (distPhase ma) (distArg bp),
            postPhases bp
          ]
      )

data GnuBuildCfg b h t a = GnuBuildCfg
  { mkDerivArg :: MkDerivationArg a,
    multiOutputDocName :: Text,
    multiOutput :: Maybe (MultiOutput OutputName -> MultiOutput OutputName),
    nativeLibs :: SimpleDeps [] NativeLib b h t,
    executables :: SimpleDeps [] Executable b h t,
    extraNativeLibs :: [(SimpleDeps [] NativeLib b h t, DrvM SetupHook)],
    setupHooks :: [DrvM SetupHook],
    builderCfg :: BuilderCfg b
  }

gnuBuildSystem ::
  (SingI b, SingI h, SingI t, NamedHashAlgo a) =>
  StdEnv s b h t ->
  GnuBuildCfg b h t a ->
  ( HM.HashMap OutputName (Code HsQ FilePath) ->
    Maybe (MultiOutput OutputName) ->
    PhaseBuildInfo ->
    ModuleM 'MainModule (MakeCfg, GnuBuildPhases h t)
  ) ->
  ExtendDeriv h t
gnuBuildSystem std gbc gbpF =
  let out = outputOpts (mkDerivArg gbc)
      multiCfg =
        multiOutput gbc
          <*> Just (defMultiOutput (multiOutputDocName gbc) out)
      execDirs = executableDirs (executables gbc <> stdEnvExec std)
      (nLib, mSt) = collectDepsA (nativeLibs gbc) (extraNativeLibs gbc)
   in mkDerivation
        std
        (mkDerivArg gbc)
        BuilderArg
          { buildEnv = do
              execEnv <-
                fmap
                  (\(pb, ph, pt) -> executableEnv pb ph pt)
                  execDirs
              nativeLibEnv <- setupNativeLib nLib
              nativeLibSH <- mSt
              setupH <- sequenceA (setupHooks gbc)
              pure
                ( let sh =
                        mconcat
                          ( execEnv
                              : nativeLibEnv
                              : nativeLibSH
                              : setupH
                          )
                   in BuildEnv
                        { beEnv = newEnv sh,
                          bePassAsFile = newPassAsFile sh
                        }
                ),
            buildCfg = builderCfg gbc,
            builderExt = [],
            builderMain = do
              om <- useModule (outputMod out)
              let pbi =
                    PhaseBuildInfo
                      { nativeLibSearchDir =
                          liftA2
                            (++)
                            (nativeLibPaths (depsHostHost nLib))
                            (nativeLibPaths (depsHostTarget nLib)),
                        pathEnv = execDirs,
                        multiOutputArg =
                          fmap
                            (\m -> (m, getMultiOutPath om m))
                            multiCfg,
                        outputs = om
                      }
              (mc, gbp) <- gbpF om multiCfg pbi
              buildPhases pbi mc gbp
          }