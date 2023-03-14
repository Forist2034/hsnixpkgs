module Lib.HsNixPkgs (
  name,
  hsnixpkgs,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as S
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.Version
import HsCabal
import Language.Haskell.Extension
import qualified Lib.GenPkgs as GP
import System.Directory
import System.FilePath
import Utils

baseDep :: Dependency
baseDep = "base" ^>= [4, 17]

textDep :: Dependency
textDep = "text" ^>= [2, 0]

unorderedContainersDep :: Dependency
unorderedContainersDep = "unordered-containers" ^>= [0, 2]

mtlDep :: Dependency
mtlDep = "mtl" ^>= [2, 2]

singletonDep :: [Dependency]
singletonDep =
  [ "singletons" ^>= [3, 0],
    "singletons-base" ^>= [3, 1]
  ]

ghcOptions :: PerCompilerFlavor [String]
ghcOptions = PerCompilerFlavor ["-fenable-th-splice-warnings"] []

name :: PackageName
name = "hsnixpkgs"

systemLib :: IO Library
systemLib = do
  let srcRoot = "system"
  mods <- listMod "hs" srcRoot
  let exposedM = "HsNixPkgs.System"
  pure
    ( emptyLibrary
        { libName = LSubLibName "system",
          exposedModules = [exposedM],
          libBuildInfo =
            (simpleBuildInfo srcRoot mempty)
              { defaultExtensions =
                  fmap
                    EnableExtension
                    ( OverloadedStrings
                        : [ DataKinds,
                            DefaultSignatures,
                            EmptyCase,
                            ExistentialQuantification,
                            FlexibleContexts,
                            FlexibleInstances,
                            GADTs,
                            InstanceSigs,
                            KindSignatures,
                            PolyKinds,
                            RankNTypes,
                            ScopedTypeVariables,
                            StandaloneDeriving,
                            StandaloneKindSignatures,
                            TemplateHaskell,
                            TypeApplications,
                            TypeFamilies,
                            TypeOperators,
                            UndecidableInstances
                          ]
                    )
                    ++ fmap
                      DisableExtension
                      [CUSKs, NamedWildCards, StarIsType],
                otherModules = S.toAscList (S.delete exposedM mods),
                targetBuildDepends =
                  [ baseDep,
                    textDep,
                    anyVersionDep "template-haskell",
                    Dependency
                      "hsnix-core"
                      anyVersion
                      (NES.singleton (LSubLibName "core-types"))
                  ]
                    ++ singletonDep
              }
        }
    )

dependentLib :: Library -> IO Library
dependentLib sys = do
  let srcRoot = "dependent"
      exposedM = "HsNixPkgs.Dependent"
  mods <- listMod "hs" srcRoot
  pure
    ( emptyLibrary
        { libName = LSubLibName "dependent",
          exposedModules = [exposedM],
          libBuildInfo =
            (simpleBuildInfo srcRoot ghcOptions)
              { defaultExtensions = [EnableExtension QuantifiedConstraints],
                otherModules = S.toAscList (S.delete exposedM mods),
                targetBuildDepends =
                  [ baseDep,
                    "hashable" ^>= [1, 4],
                    unorderedContainersDep,
                    "template-haskell" ^>= [2, 19],
                    mtlDep,
                    Dependency
                      name
                      anyVersion
                      (NES.singleton (libName sys))
                  ]
                    ++ singletonDep
              }
        }
    )

depstrLib :: IO Library
depstrLib =
  addLibraryMod
    ( emptyLibrary
        { libName = LSubLibName "depstr",
          libBuildInfo =
            (simpleBuildInfo "depstr" ghcOptions)
              { targetBuildDepends =
                  [ baseDep,
                    anyVersionDep "hsnix-core"
                  ]
              }
        }
    )

unpackTHLib :: IO Library
unpackTHLib =
  addLibraryMod
    ( emptyLibrary
        { libName = LSubLibName "stdenv-unpack-th",
          libBuildInfo =
            (simpleBuildInfo ("stdenv" </> "unpack-th") ghcOptions)
              { targetBuildDepends =
                  [ baseDep,
                    textDep,
                    unorderedContainersDep,
                    "yaml" ^>= [0, 11],
                    "template-haskell" ^>= [2, 19],
                    Dependency
                      "hsnixpkgs-stdenv-pack"
                      anyVersion
                      (NES.singleton (LSubLibName "types")),
                    Dependency
                      "hsnix-core"
                      anyVersion
                      (NES.singleton (LSubLibName "core-types"))
                  ]
              }
        }
    )

mainLib :: NonEmpty Library -> IO (PackageFlag, CondTree ConfVar [a] Library)
mainLib subL = do
  baseL <-
    addLibraryMod
      ( emptyLibrary
          { libBuildInfo =
              (simpleBuildInfo "src" ghcOptions)
                { defaultExtensions =
                    EnableExtension
                      <$> [ OverloadedStrings,
                            ApplicativeDo,
                            DataKinds,
                            KindSignatures,
                            ScopedTypeVariables,
                            TypeApplications
                          ],
                  targetBuildDepends =
                    [ "base" ^>= [4, 17],
                      "text" ^>= [2, 0],
                      "bytestring" ^>= [0, 11],
                      "containers" ^>= [0, 6],
                      "unordered-containers" ^>= [0, 2],
                      "hashable" ^>= [1, 4],
                      "data-default" ^>= [0, 7],
                      "mtl" ^>= [2, 2],
                      "template-haskell" ^>= [2, 19],
                      "filepath" ^>= [1, 4],
                      "directory" ^>= [1, 3],
                      anyVersionDep "hsnixpkgs-builderlib-boot",
                      anyVersionDep "hsnix-core",
                      anyVersionDep GP.name,
                      Dependency
                        name
                        anyVersion
                        (NES.fromNonEmpty (libName <$> subL))
                    ]
                      ++ singletonDep
                }
          }
      )
  let flag =
        (emptyFlag "dev")
          { flagDescription = "Is in dev environment (add backend to build-depends to make HLS working)",
            flagDefault = False,
            flagManual = False
          }
  pure
    ( flag,
      CondNode
        { condTreeData = baseL,
          condTreeConstraints = [],
          condTreeComponents =
            [ condIfThen
                (Var (PackageFlag (flagName flag)))
                ( unConditional
                    ( emptyLibrary
                        { libBuildInfo =
                            emptyBuildInfo
                              { targetBuildDepends = [anyVersionDep "hsnix-drv"]
                              }
                        }
                    )
                )
            ]
        }
    )

hsnixpkgs :: IO LocalPackage
hsnixpkgs =
  let root = "hsnixpkgs"
   in withCurrentDirectory root $ do
        stdEnvInfo <-
          S.mapMonotonic (\p -> "stdenv" </> "spec" </> p)
            <$> listDirRec ("stdenv" </> "spec")
        system <- systemLib
        dependent <- dependentLib system
        depstr <- depstrLib
        unpackTH <- unpackTHLib
        (devFlag, lib) <- mainLib (system :| [dependent, depstr, unpackTH])
        pure
          ( LocalPackage
              { lpName = name,
                lpRoot = root,
                lpDesc =
                  emptyGenericPackageDescription
                    { packageDescription =
                        (simplePkgDesc name [0, 1, 0, 0])
                          { extraSrcFiles = S.toAscList stdEnvInfo
                          },
                      genPackageFlags = [devFlag],
                      condLibrary = Just lib,
                      condSubLibraries =
                        unConditionalSubLib
                          <$> [ system,
                                dependent,
                                depstr,
                                unpackTH
                              ]
                    }
              }
          )