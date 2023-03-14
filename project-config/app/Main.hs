module Main (main) where

import Data.Foldable
import Distribution.Client.ProjectConfig
import Distribution.Client.Types.AllowNewer
import Distribution.Client.Types.SourceRepo
import Distribution.Client.VCS
import Distribution.PackageDescription
import HsCabal
import Lib.BuilderLib
import Lib.GenPkgs hiding (name)
import qualified Lib.HsNixPkgs as MainLib
import Lib.StdEnv
import System.Directory
import Utils

compileBuilder :: LocalPackage
compileBuilder =
  let name = "compile-builder"
      root = "compile-builder"
      exe =
        emptyExecutable
          { exeName = "compile-builder",
            modulePath = "Main.hs",
            buildInfo =
              (simpleBuildInfo "app" mempty)
                { targetBuildDepends =
                    [ "base" ^>= [4, 17],
                      "filepath" ^>= [1, 4],
                      "directory" ^>= [1, 3],
                      "process" ^>= [1, 6]
                    ]
                }
          }
   in LocalPackage
        { lpName = name,
          lpRoot = root,
          lpDesc =
            emptyGenericPackageDescription
              { packageDescription = simplePkgDesc name [0, 1, 0, 0],
                condExecutables = [(exeName exe, unConditional exe)]
              }
        }

main :: IO ()
main =
  withCurrentDirectory ".." $ do
    pkgs <-
      (compileBuilder :)
        <$> sequence
          [ stdEnvPack,
            stdenvUnpack,
            builderLibBoot,
            genPkgs,
            MainLib.hsnixpkgs
          ]
    traverse_ (writeLocalPackage ".") pkgs
    writeProjectConfigFile
      "cabal.project"
      ( (simpleCabalProject pkgs)
          { projectPackagesRepo =
              [ SourceRepositoryPackage
                  { srpType = vcsRepoType vcsGit,
                    srpLocation = "https://github.com/Forist2034/nix-core",
                    srpTag = Just "0ea47cacbc4cb494211bb051fed24e2fd52baf05",
                    srpBranch = Nothing,
                    srpSubdir = ["nix-archive", "hsnix-core", "hsnix-drv"],
                    srpCommand = []
                  }
              ],
            projectConfigShared =
              mempty
                { projectConfigAllowNewer =
                    Just
                      ( AllowNewer
                          ( RelaxDepsSome
                              ( RelaxedDep
                                  (RelaxDepScopePackage "relude")
                                  RelaxDepModNone
                                  . RelaxDepSubjectPkg
                                  <$> [ "base",
                                        "ghc-prim"
                                      ]
                              )
                          )
                      )
                }
          }
      )
