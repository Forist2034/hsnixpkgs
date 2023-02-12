{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Develop.BuildSystem.Gnu
  ( PhaseBuildInfo,
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
  )
where

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
import HsNix.Builtin.AddFile
import HsNix.Hash (NamedHashAlgo)
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
import HsNixPkgs.HsBuilder.Generate
import HsNixPkgs.HsBuilder.OutputMod
import HsNixPkgs.SetupHook
import HsNixPkgs.SetupHook.Executable
import HsNixPkgs.StdEnv.MakeDeriv
import HsNixPkgs.StdEnv.StdEnv
import HsNixPkgs.System
import HsNixPkgs.Util
import Language.Haskell.TH hiding (Phases)
import Language.Haskell.TH.Syntax hiding (Phases)

type PhaseM m = DepModM 'MainModule m (Code HsQ Phase)

type ModExprM m = DepModM 'MainModule m (Code HsQ (BIO ()))

type MkPhase m = Code HsQ (BIO ()) -> HsQ [Dec] -> PhaseM m

mkDefPhDecl :: ModExprM m -> MkPhase m -> PhaseM m
mkDefPhDecl me f = do
  e <- me
  f e (pure [])

data PhaseBuildInfo m = PhaseBuildInfo
  { multiOutputArg :: Maybe (MultiOutput Text, MultiOutput (Code HsQ FilePath)),
    nativeLibSearchDir :: m [DrvStr m],
    pathEnv :: m ([DrvStrBuilder m], [DrvStrBuilder m], [DrvStrBuilder m]),
    outputs :: HM.HashMap Text (Code HsQ FilePath)
  }

data UnpackPhase m = UnpackPhase
  { unpackSources :: SourceList m,
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

mkConfigurePhase :: MkPhase m
mkConfigurePhase = newPhase "configurePhase" "configuring"

configureExpr ::
  ApplicativeDeriv m =>
  PhaseBuildInfo m ->
  ConfigurePhase h t ->
  ModExprM m
configureExpr pbi cp@ConfigurePhase {configureCfg = cfg} = do
  msp <-
    if fixLibtool cfg
      then
        (\v -> [|Just $(varE v)|])
          <$> useModule libSearchMod
      else pure [|Nothing|]
  pure
    ( runHook
        (configureHook cfg)
        ( unsafeCodeCoerce
            [|
              B.configureFunc
                $(lift (configureScript cfg))
                $configFlags
                $msp
                $( case configPrefix cp of
                     Just p -> [|Just $(unTypeCode p)|]
                     Nothing -> [|Nothing|]
                 )
              |]
        )
    )
  where
    libSearchMod =
      stringModule
        "Configure.LibSearchPath"
        ( DepStr
            ( fmap
                ( \s ->
                    toDrvStr
                      ( unlinesDSB
                          [ "{-# LANGUAGE OverloadedStrings #-}",
                            "module Configure.LibSearchPath where",
                            mempty,
                            "libSearchPath :: Text",
                            "libSearchPath = \""
                              <> unwordsDSB (fmap fromDrvStr s)
                              <> "\""
                          ]
                      )
                )
                (nativeLibSearchDir pbi)
            )
        )
        ["base", "text"]
        ( Name
            (OccName "libSearchPath")
            (NameQ (ModName "Configure.LibSearchPath"))
        )
    configFlags :: HsQ Exp
    configFlags =
      listE
        ( concat
            [ case configPrefix cp of
                Just p ->
                  [ [|
                      $(lift (concat ["--", prefixKey cfg, "="]))
                        ++ $(unTypeCode p)
                      |]
                  ]
                Nothing -> [],
              fmap
                (\(n, v) -> lift (concat ["--", n, "=", v]))
                (configurePlatform cfg),
              case multiOutputArg pbi of
                Just (_, moa) ->
                  [ [|"--bindir=" ++ $(unTypeCode (outBin moa)) ++ "/bin"|],
                    [|"--sbindir=" ++ $(unTypeCode (outBin moa)) ++ "/sbin"|],
                    [|"--includedir=" ++ $(unTypeCode (outInclude moa)) ++ "/include"|],
                    [|"--oldincludedir=" ++ $(unTypeCode (outInclude moa)) ++ "/include"|],
                    [|"--mandir=" ++ $(unTypeCode (outMan moa)) ++ "/share/man"|],
                    [|"--infodir=" ++ $(unTypeCode (outInfo moa)) ++ "/share/info"|],
                    [|
                      "--docdir="
                        ++ $(unTypeCode (outDoc moa))
                        ++ "/share/doc/"
                        ++ $(lift (shareDocName moa))
                      |],
                    [|"--libdir=" ++ $(unTypeCode (outLib moa)) ++ "/lib"|],
                    [|"--libexecdir=" ++ $(unTypeCode (outLib moa)) ++ "/libexec"|],
                    [|"--localedir=" ++ $(unTypeCode (outLib moa)) ++ "/share/locale"|]
                  ]
                Nothing -> [],
              fmap unTypeCode (configureFlags cfg)
            ]
        )

configurePhase ::
  ApplicativeDeriv m =>
  PhaseBuildInfo m ->
  ConfigurePhase h t ->
  PhaseM m
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

mkBuildPhase :: MkPhase m
mkBuildPhase = newPhase "buildPhase" "building"

buildExpr :: MakeCfg -> BuildCfg -> Code HsQ (BIO ())
buildExpr mc cfg =
  runHook
    (buildHook cfg)
    ( unsafeCodeCoerce
        [|
          B.runMake
            "build"
            $(listE (fmap unTypeCode (buildFlags cfg)))
            $( lift
                 B.MakeCfg
                   { B.mcMakeFile = makeFile mc,
                     B.mcEnableParallel = buildInParallel cfg
                   }
             )
          |]
    )

buildPhase :: MakeCfg -> BuildCfg -> PhaseM m
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

mkCheckPhase :: MkPhase m
mkCheckPhase = newPhase "checkPhase" "running tests"

checkExpr :: MakeCfg -> CheckPhase -> Code HsQ (BIO ())
checkExpr mc cp@CheckPhase {checkCfg = cfg} =
  runHook
    (checkHook cfg)
    ( unsafeCodeCoerce
        [|
          B.runMake
            "check"
            $( listE
                 ( fmap unTypeCode (checkFlags cfg)
                     ++ fmap lift (checkTarget cp)
                 )
             )
            $( lift
                 B.MakeCfg
                   { B.mcMakeFile = makeFile mc,
                     B.mcEnableParallel = checkInParallel cfg
                   }
             )
          |]
    )

checkPhase :: MakeCfg -> CheckPhase -> PhaseM m
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

mkInstallPhase :: MkPhase m
mkInstallPhase = newPhase "installPhase" "install"

installExpr :: PhaseBuildInfo m -> MakeCfg -> InstallPhase -> Code HsQ (BIO ())
installExpr pbi mc ip@InstallPhase {installCfg = cfg} =
  runHook
    (installHook cfg)
    ( unsafeCodeCoerce
        [|
          B.installFunc
            $(listE (unTypeCode <$> installDest ip))
            $( listE
                 ( concat
                     [ case multiOutputArg pbi of
                         Just (_, moa) ->
                           let dev = unTypeCode (outDev moa)
                            in [ [|"pkgconfigdir=" ++ $dev ++ "/lib/pkgconfig"|],
                                 [|"m4datadir=" ++ $dev ++ "/share/aclocal"|],
                                 [|"aclocaldir=" ++ $dev ++ "/share/aclocal"|]
                               ]
                         Nothing -> [],
                       fmap unTypeCode (installFlags cfg),
                       fmap lift (installTarget ip)
                     ]
                 )
             )
            $( lift
                 B.MakeCfg
                   { B.mcMakeFile = makeFile mc,
                     B.mcEnableParallel = False
                   }
             )
          |]
    )

installPhase ::
  PhaseBuildInfo m ->
  MakeCfg ->
  InstallPhase ->
  PhaseM m
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
  (SingI h, SingI t, ApplicativeDeriv m) =>
  PhaseBuildInfo m ->
  SimpleFixupCfg h t ->
  DepModM 'MainModule m [FixupTarget]
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
            ( DepStr
                ( fmap
                    ( \(pb, ph, _) ->
                        toDrvStr
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
                )
            )
            []
            ( Name (OccName "path") (NameQ (ModName modName)),
              Name (OccName "hostPath") (NameQ (ModName modName))
            )
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

mkInstallCheckPhase :: MkPhase m
mkInstallCheckPhase = newPhase "installCheckPhase" "running install tests"

installCheckExpr :: MakeCfg -> InstallCheckPhase -> Code HsQ (BIO ())
installCheckExpr mc icp@InstallCheckPhase {installCheckCfg = cfg} =
  runHook
    (installCheckHook cfg)
    ( unsafeCodeCoerce
        [|
          B.runMake
            "install check"
            $( listE
                 ( fmap unTypeCode (installCheckFlags cfg)
                     ++ fmap lift (installCheckTargets icp)
                 )
             )
            $( lift
                 B.MakeCfg
                   { B.mcMakeFile = makeFile mc,
                     B.mcEnableParallel = installCheckInParallel cfg
                   }
             )
          |]
    )

installCheckPhase :: MakeCfg -> InstallCheckPhase -> PhaseM m
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

mkDistPhase :: MkPhase m
mkDistPhase = newPhase "distPhase" "distPhase"

distExpr :: MakeCfg -> DistPhase -> Code HsQ (BIO ())
distExpr mc dp@DistPhase {distCfg = cfg} =
  runHook
    (distHook cfg)
    ( unsafeCodeCoerce
        [|
          B.distFunc
            $( listE
                 ( fmap unTypeCode (distFlags cfg)
                     ++ fmap lift (distTargets dp)
                 )
             )
            $( lift
                 B.MakeCfg
                   { B.mcMakeFile = makeFile mc,
                     B.mcEnableParallel = False
                   }
             )
            $( case distTarballs dp of
                 Just (d, t) -> [|Just ($(unTypeCode d), $(lift t))|]
                 Nothing -> [|Nothing|]
             )
          |]
    )

distPhase :: MakeCfg -> DistPhase -> PhaseM m
distPhase mc dp = mkDistPhase (distExpr mc dp) (pure [])

type Phases m = [PhaseM m]

type DefPhase m a = Either (Phases m) a

data GnuBuildPhases h t m = GnuBuildPhases
  { prePhases :: Phases m,
    unpackArg :: DefPhase m (UnpackPhase m),
    patchArg :: DefPhase m (NEL.NonEmpty (Patch m), PatchCfg),
    preConfigurePhases :: Phases m,
    configureArg :: DefPhase m (ConfigurePhase h t),
    preBuildPhases :: Phases m,
    buildArg :: DefPhase m BuildCfg,
    checkArg :: DefPhase m CheckPhase,
    preInstallPhases :: Phases m,
    installArg :: DefPhase m InstallPhase,
    fixupArg :: DefPhase m (Hook FixupP, SimpleFixupCfg h t),
    installCheckArg :: DefPhase m InstallCheckPhase,
    preDistPhases :: Phases m,
    distArg :: DefPhase m DistPhase,
    postPhases :: Phases m
  }

simpleGnuBuildPhases ::
  DefPhase m (UnpackPhase m) ->
  DefPhase m (ConfigurePhase h t) ->
  DefPhase m InstallPhase ->
  GnuBuildPhases h t m
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

toPhases :: (a -> PhaseM m) -> DefPhase m a -> Phases m
toPhases f = either id (L.singleton . f)

buildPhases ::
  (SingI h, SingI t, BuiltinAddText m) =>
  PhaseBuildInfo m ->
  MakeCfg ->
  GnuBuildPhases h t m ->
  DepModM 'MainModule m (Code HsQ (BIO ()))
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

data GnuBuildCfg b h t a m = GnuBuildCfg
  { mkDerivArg :: MkDerivationArg a m,
    multiOutputDocName :: Text,
    multiOutput :: Maybe (MultiOutput Text -> MultiOutput Text),
    nativeLibs :: SimpleDeps [] NativeLib b h t m,
    executables :: SimpleDeps [] Executable b h t m,
    extraNativeLibs :: [(SimpleDeps [] NativeLib b h t m, m (SetupHook m))],
    setupHooks :: [m (SetupHook m)],
    builderCfg :: BuilderCfg b m
  }

gnuBuildSystem ::
  (SingI b, SingI h, SingI t, BuiltinAddText m, NamedHashAlgo a) =>
  StdEnv s b h t m ->
  GnuBuildCfg b h t a m ->
  ( HM.HashMap Text (Code HsQ FilePath) ->
    Maybe (MultiOutput Text) ->
    PhaseBuildInfo m ->
    DepModM 'MainModule m (MakeCfg, GnuBuildPhases h t m)
  ) ->
  ExtendDeriv h t m
gnuBuildSystem std gbc gbpF =
  let out = outputOpts (mkDerivArg gbc)
      multiCfg =
        multiOutput gbc
          <*> Just
            (defMultiOutput (multiOutputDocName gbc) out)
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