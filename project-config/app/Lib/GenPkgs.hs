module Lib.GenPkgs (name, genPkgs) where

import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.PackageDescription
import Distribution.Version
import HsCabal
import Language.Haskell.Extension
import System.Directory
import Utils

name :: PackageName
name = "haskell-gen-package"

genPkgs :: IO LocalPackage
genPkgs =
  let root = "haskell-gen-package"
      commonDeps =
        [ "base" ^>= [4, 17],
          "unordered-containers" ^>= [0, 2],
          "mtl" ^>= [2, 2],
          "template-haskell" ^>= [2, 19]
        ]
   in withCurrentDirectory root $ do
        procName <-
          addLibraryMod
            ( emptyLibrary
                { libName = LSubLibName "proc-name",
                  libBuildInfo =
                    (simpleBuildInfo "proc-name" mempty)
                      { targetBuildDepends =
                          commonDeps
                            ++ [ "containers" ^>= [0, 6]
                               ]
                      }
                }
            )
        lib <-
          addLibraryMod
            ( emptyLibrary
                { libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { otherModules = ["Language.Haskell.GenPackage.Types"],
                        defaultExtensions =
                          [ EnableExtension KindSignatures,
                            EnableExtension DataKinds
                          ],
                        targetBuildDepends =
                          commonDeps
                            ++ [ Dependency
                                  name
                                  anyVersion
                                  (NES.singleton (libName procName))
                               ]
                      }
                }
            )
        pure
          ( LocalPackage
              { lpName = name,
                lpRoot = root,
                lpDesc =
                  emptyGenericPackageDescription
                    { packageDescription = simplePkgDesc name [0, 1, 0, 0],
                      condLibrary = Just (unConditional lib),
                      condSubLibraries = [unConditionalSubLib procName]
                    }
              }
          )