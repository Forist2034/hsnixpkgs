module Lib.BuilderLib where

import Distribution.PackageDescription
import HsCabal
import Language.Haskell.Extension
import System.Directory
import System.FilePath
import Utils

builderLibBoot :: IO LocalPackage
builderLibBoot =
  let name = "hsnixpkgs-builderlib-boot"
      root = "builderlib" </> "boot"
   in withCurrentDirectory root $ do
        lib <-
          addLibraryMod
            ( emptyLibrary
                { libBuildInfo =
                    (simpleBuildInfo "src" mempty)
                      { defaultExtensions = [EnableExtension OverloadedStrings],
                        targetBuildDepends =
                          [ "base" ^>= [4, 17],
                            "text" ^>= [2, 0],
                            "bytestring" ^>= [0, 11],
                            "filepath" ^>= [1, 4],
                            "directory" ^>= [1, 3],
                            "process" ^>= [1, 6],
                            "time" ^>= [1, 9],
                            "unix" ^>= [2, 8],
                            "mtl" ^>= [2, 2],
                            "exceptions" ^>= [0, 10],
                            "deepseq" ^>= [1, 4],
                            anyVersionDep "template-haskell"
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
                      condLibrary = Just (unConditional lib)
                    }
              }
          )