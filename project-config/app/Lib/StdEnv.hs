module Lib.StdEnv (stdEnvPack, stdenvUnpack) where

import Distribution.PackageDescription
import qualified Distribution.System as Sys
import Distribution.Version
import HsCabal
import HsCabal.QQ
import Language.Haskell.Extension
import System.Directory
import System.FilePath
import Utils

condOSExe :: Sys.OS -> Executable -> CondTree ConfVar [Dependency] Executable
condOSExe os v =
  CondNode
    { condTreeData = v,
      condTreeConstraints = [],
      condTreeComponents =
        [ condIfThen
            (CNot (Var (OS os)))
            ( unConditional
                ( emptyExecutable
                    { buildInfo =
                        emptyBuildInfo
                          { buildable = False
                          }
                    }
                )
            )
        ]
    }

stdEnvPack :: IO LocalPackage
stdEnvPack =
  let name = "hsnixpkgs-stdenv-pack"
      root = "stdenv" </> "pack"
      commonDeps =
        [ "base" ^>= [4, 17],
          "text" @@ [versionRangeQ| >= 1.2 && < 2.1 |],
          "bytestring" @@ [versionRangeQ| >= 0.10 && < 0.12 |],
          "filepath" ^>= [1, 4, 100],
          "directory" ^>= [1, 3, 8]
        ]
      commonExts =
        [ EnableExtension OverloadedStrings,
          EnableExtension Strict
        ]
   in withCurrentDirectory root $ do
        lib <-
          addLibraryMod
            ( emptyLibrary
                { libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { defaultExtensions = commonExts,
                        targetBuildDepends =
                          commonDeps
                            ++ [ "mtl" ^>= [2, 2],
                                 "hashable" ^>= [1, 4, 2],
                                 "unordered-containers" ^>= [0, 2],
                                 anyVersionDep "lzma",
                                 "binary" ^>= [0, 8],
                                 "aeson" ^>= [2, 1],
                                 "cryptonite" @@ [versionRangeQ| >= 0.29 && < 0.31 |],
                                 anyVersionDep "nix-archive"
                               ]
                      }
                }
            )
        let linuxExe =
              emptyExecutable
                { exeName = "pack-stdenv-linux",
                  modulePath = "Main.hs",
                  buildInfo =
                    (simpleBuildInfo ("app" </> "linux") mempty)
                      { defaultExtensions = commonExts,
                        targetBuildDepends =
                          commonDeps
                            ++ [ "yaml" ^>= [0, 11],
                                 Dependency name anyVersion mainLibSet
                               ]
                      }
                }
        pure
          ( LocalPackage
              { lpName = name,
                lpRoot = root,
                lpDesc =
                  emptyGenericPackageDescription
                    { packageDescription = simplePkgDesc name [0, 1, 0, 0],
                      condLibrary = Just (unConditional lib),
                      condExecutables =
                        [ (exeName linuxExe, condOSExe Sys.Linux linuxExe)
                        ]
                    }
              }
          )

stdenvUnpack :: IO LocalPackage
stdenvUnpack =
  let name = "hsnixpkgs-stdenv-unpack"
      root = "stdenv" </> "unpack"
      commonDeps =
        [ "base" ^>= [4, 17],
          "text" ^>= [2, 0],
          "bytestring" ^>= [0, 11]
        ]
      commonExts =
        [ EnableExtension OverloadedStrings,
          EnableExtension Strict
        ]
   in withCurrentDirectory root $ do
        lib <-
          addLibraryMod
            ( emptyLibrary
                { libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { defaultExtensions = commonExts,
                        includeDirs = ["include"],
                        cSources = ["cbits/rewrite.c"],
                        ccOptions = ["-O3"],
                        targetBuildDepends = commonDeps
                      }
                }
            )
        let linuxExe =
              emptyExecutable
                { exeName = "stdenv-unpack-linux",
                  modulePath = "Main.hs",
                  buildInfo =
                    (simpleBuildInfo ("app" </> "linux") mempty)
                      { targetBuildDepends =
                          commonDeps
                            ++ [ anyVersionDep "nix-archive",
                                 anyVersionDep "lzma",
                                 "binary" ^>= [0, 8],
                                 "filepath" ^>= [1, 4, 100],
                                 Dependency name anyVersion mainLibSet
                               ]
                      }
                }
        pure
          ( LocalPackage
              { lpName = name,
                lpRoot = root,
                lpDesc =
                  emptyGenericPackageDescription
                    { packageDescription = simplePkgDesc name [0, 1, 0, 0],
                      condLibrary = Just (unConditional lib),
                      condExecutables =
                        [ (exeName linuxExe, condOSExe Sys.Linux linuxExe)
                        ]
                    }
              }
          )